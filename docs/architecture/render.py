import sys
from playwright.sync_api import sync_playwright
src, out = sys.argv[1], sys.argv[2]
# Renders each diagram in architecture.html to a PNG:
#   python docs/architecture/render.py docs/architecture/architecture.html docs/architecture
names = ["agent", "spoken-turn", "forum", "improvement-loop", "injection-chain"]
with sync_playwright() as p:
    b = p.chromium.launch(channel="msedge")
    pg = b.new_page(viewport={"width": 1280, "height": 900}, device_scale_factor=2, color_scheme="light")
    pg.goto("file:///" + src)
    pg.wait_for_timeout(1500)
    figs = pg.locator("figure svg")
    print(figs.count())
    for i in range(figs.count()):
        figs.nth(i).screenshot(path=f"{out}/{names[i] if i < len(names) else i}.png")
    b.close()
