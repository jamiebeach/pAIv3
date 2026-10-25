# pAI-Eyes: host-local perception for the crew (bead v3-byw).
# Build:  docker build -t pai-eyes:local services/eyes
# Run:    docker run -d --name pai-eyes --gpus all --restart unless-stopped \
#           -p 127.0.0.1:8470:8470 -v pai-eyes-data:/data pai-eyes:local
FROM python:3.12-slim

RUN pip install --no-cache-dir "onnxruntime-gpu[cuda,cudnn]==1.21.1" "opencv-python-headless==4.10.0.84" "numpy==2.1.3"

# YOLOX-S, Apache-2.0 (Megvii-BaseDetection/YOLOX release 0.1.1rc0), pinned by checksum.
ADD --chmod=644 --checksum=sha256:c5c2d13e59ae883e6af3b45daea64af4833a4951c92d116ec270d9ddbe998063 https://github.com/Megvii-BaseDetection/YOLOX/releases/download/0.1.1rc0/yolox_s.onnx /models/yolox_s.onnx

RUN chmod 755 /models && chmod 644 /models/yolox_s.onnx
COPY eyes.py detector.py /app/
WORKDIR /app
USER 1000
EXPOSE 8470
CMD ["python", "-u", "eyes.py"]
