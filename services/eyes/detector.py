"""Person detection for pAI-Eyes: YOLOX-S (Apache-2.0) through ONNX Runtime.

people(jpeg) -> [[x1, y1, x2, y2, score], ...] for the COCO "person" class,
in the frame's own pixels. The GPU is used when ONNX Runtime offers CUDA;
otherwise the CPU, at the cost of time, not correctness.
"""
import os

import cv2
import numpy as np
import onnxruntime as ort

MODEL_PATH = os.environ.get("EYES_MODEL", "/models/yolox_s.onnx")
MODEL_ID = "yolox-s-onnx"
INPUT = 640
SCORE = float(os.environ.get("EYES_PERSON_SCORE", "0.5"))
NMS_IOU = 0.45

_session = None
_grid = None


def load():
    global _session, _grid
    if hasattr(ort, "preload_dlls"):
        ort.preload_dlls()  # the CUDA and cuDNN libraries pip installed beside onnxruntime
    providers = [p for p in ("CUDAExecutionProvider", "CPUExecutionProvider") if p in ort.get_available_providers()]
    _session = ort.InferenceSession(MODEL_PATH, providers=providers)
    grids, strides = [], []
    for stride in (8, 16, 32):
        size = INPUT // stride
        ys, xs = np.meshgrid(np.arange(size), np.arange(size), indexing="ij")
        grids.append(np.stack((xs, ys), 2).reshape(-1, 2))
        strides.append(np.full((size * size, 1), stride))
    _grid = (np.concatenate(grids).astype(np.float32), np.concatenate(strides).astype(np.float32))


def provider():
    return _session.get_providers()[0] if _session else "not loaded"


def _letterbox(image):
    h, w = image.shape[:2]
    ratio = min(INPUT / h, INPUT / w)
    resized = cv2.resize(image, (int(w * ratio), int(h * ratio)), interpolation=cv2.INTER_LINEAR)
    padded = np.full((INPUT, INPUT, 3), 114, dtype=np.uint8)
    padded[:resized.shape[0], :resized.shape[1]] = resized
    return padded, ratio


def iou(a, b):
    x1, y1 = max(a[0], b[0]), max(a[1], b[1])
    x2, y2 = min(a[2], b[2]), min(a[3], b[3])
    inter = max(0.0, x2 - x1) * max(0.0, y2 - y1)
    union = (a[2] - a[0]) * (a[3] - a[1]) + (b[2] - b[0]) * (b[3] - b[1]) - inter
    return inter / union if union > 0 else 0.0


def people(jpeg):
    image = cv2.imdecode(np.frombuffer(jpeg, np.uint8), cv2.IMREAD_COLOR)
    if image is None:
        raise ValueError("not a decodable image")
    padded, ratio = _letterbox(image)
    blob = padded.transpose(2, 0, 1)[None].astype(np.float32)
    out = _session.run(None, {_session.get_inputs()[0].name: blob})[0][0]
    grid, stride = _grid
    xy = (out[:, :2] + grid) * stride
    wh = np.exp(out[:, 2:4]) * stride
    scores = out[:, 4] * out[:, 5]          # objectness times the "person" class
    keep = scores >= SCORE
    if not keep.any():
        return []
    xy, wh, scores = xy[keep], wh[keep], scores[keep]
    boxes = np.concatenate((xy - wh / 2, xy + wh / 2), 1) / ratio
    order = cv2.dnn.NMSBoxes([[float(x1), float(y1), float(x2 - x1), float(y2 - y1)] for x1, y1, x2, y2 in boxes],
                             scores.tolist(), SCORE, NMS_IOU)
    return [[*map(float, boxes[i]), float(scores[i])] for i in np.array(order).flatten()]
