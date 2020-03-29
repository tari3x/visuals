const app = new PIXI.Application({ antialias: true });
document.body.appendChild(app.view);

const graphics = new PIXI.Graphics();

app.stage.addChild(graphics);

// app.renderer.resize(window.innerWidth, window.innerHeight);
app.resizeTo = window;
app.resize ();

console.log (app.view.width);

// graphics.beginFill(1000);
graphics.lineStyle({ width : 100, color :  0x334455 });
graphics.drawCircle(800, 250, 100);
graphics.endFill();
