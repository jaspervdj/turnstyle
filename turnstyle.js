"use strict";

class Rational {
    constructor(numerator, denominator) {
        const gcd = Rational._gcd(numerator, denominator);
        this._num = numerator / gcd;
        this._denom = denominator / gcd;
    }

    add(other) {
        return new Rational(
            this._num * other._denom + other._num * this._denom,
            this._denom * other._denom,
        );
    }

    subtract(other) {
        return new Rational(
            this._num * other._denom - other._num * this._denom,
            this._denom * other._denom,
        );
    }

    multiply(other) {
        return new Rational(this._num * other._num, this._denom * other._denom);
    }

    divide(other) {
        return new Rational(this._num * other._denom, this._denom * other._num);
    }

    modulo(other) {
        const lhs = this.toBigInt();
        const rhs = other.toBigInt();
        if (lhs === null || rhs === null) {
            throw new Error("Rational: modulo arguments must be integrals");
        }
        return lhs % rhs;
    }

    toBigInt() {
        if (this._denom === 1n) {
            return this._num;
        }
        return null;
    }

    toString() {
        if (this._denom === 1n) {
            return this._num.toString();
        }
        return `${this._num}/${this._denom}`;
    }

    static _gcd(a, b) {
        while (b !== 0n) {
            const tmp = a % b;
            a = b;
            b = tmp;
        }
        return a;
    }
}

class Position {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }

    right() {
        return new Position(this.x + 1, this.y);
    }

    down() {
        return new Position(this.x, this.y + 1);
    }

    left() {
        return new Position(this.x - 1, this.y);
    }

    up() {
        return new Position(this.x, this.y - 1);
    }

    neighbors() {
        return [this.right(), this.down(), this.left(), this.up()];
    }

    toString() {
        return `${this.x},${this.y}`;
    }
}

const Direction = {
    RIGHT: "RIGHT",
    DOWN:  "DOWN",
    LEFT:  "LEFT",
    UP:    "UP",
    turnLeft: (dir) => {
        switch (dir) {
            case Direction.RIGHT: return Direction.UP;
            case Direction.DOWN:  return Direction.RIGHT;
            case Direction.LEFT:  return Direction.DOWN;
            case Direction.UP:    return Direction.LEFT;
        }
    },
    turnRight: (dir) => {
        switch (dir) {
            case Direction.RIGHT: return Direction.DOWN;
            case Direction.DOWN:  return Direction.LEFT;
            case Direction.LEFT:  return Direction.UP;
            case Direction.UP:    return Direction.RIGHT;
        }
    },
};

const Pattern = {
    AAAA: "AAAA",
    AAAB: "AAAB",
    AABA: "AABA",
    AABB: "AABB",
    AABC: "AABC",
    ABAA: "ABAA",
    ABAB: "ABAB",
    ABAC: "ABAC",
    ABBA: "ABBA",
    ABBB: "ABBB",
    ABBC: "ABBC",
    ABCA: "ABCA",
    ABCB: "ABCB",
    ABCC: "ABCC",
    ABCD: "ABCD",
    parse: (a, x, y, z) => {
        if (x === a) {
            if (y === a) {
                if (z === a) {
                    return Pattern.AAAA;
                } else {
                    return Pattern.AAAB;
                }
            } else {
                if (z === a) {
                    return Pattern.AABA;
                } else if (z === y) {
                    return Pattern.AABB;
                } else {
                    return Pattern.AABC;
                }
            }
        } else {
            if (y === a) {
                if (z === a) {
                    return Pattern.ABAA;
                } else if (z === x) {
                    return Pattern.ABAB;
                } else {
                    return Pattern.ABAC;
                }
            } else if (y === x) {
                if (z === a) {
                    return Pattern.ABBA;
                } else if (z === x) {
                    return Pattern.ABBB;
                } else {
                    return Pattern.ABBC;
                }
            } else {
                if (z === a) {
                    return Pattern.ABCA;
                } else if (z === x) {
                    return Pattern.ABCB;
                } else if (z === y) {
                    return Pattern.ABCC;
                } else {
                    return Pattern.ABCD;
                }
            }
        }
    },
};

class Source {
    hex(x, y) {
        const rgba = this.rgba(x, y);
        const hr = rgba[0].toString(16).padStart(2, "0");
        const hg = rgba[1].toString(16).padStart(2, "0");
        const hb = rgba[2].toString(16).padStart(2, "0");
        const ha = rgba[3].toString(16).padStart(2, "0");
        return `#${hr}${hg}${hb}${ha}`;
    }
}

// Firefox private windows (and possibly other browsers with strict security
// settings) will apply a random noise to the data returned from
// `canvas.getImageData()`, to prevent browser fingerprinting.  This prevents
// Turnstyle programs from parsing, because colors must be exactly the same.
// This is a workaround to find similar-enough colors.
class DenoiseSource extends Source {
    constructor(src) {
        super()
        this._limit = 32;
        this._width = src.width;
        this._height = src.height;
        this._rgba = new Array(src.width * src.height);

        const palette = {};

        const sse = (as, bs) => {
            let result = 0;
            for (let i = 0; i < as.length && i < bs.length; i++) {
                result += (as[i] - bs[i]) * (as[i] - bs[i]);
            }
            return result;
        }

        const similar = (rgba) => {
            for (const k of Object.keys(palette)) {
                if (sse(rgba, palette[k]) < this._limit) {
                    return palette[k];
                }
            }
            return null;
        }

        for (let y = 0; y < src.height; y++) {
            for (let x = 0; x < src.width; x++) {
                const hex = src.hex(x, y);
                const rgba = src.rgba(x, y);
                let out;
                if (hex in palette) {
                    out = palette[hex];
                } else {
                    out = similar(rgba) || rgba;
                    palette[hex] = out;
                }
                this._rgba[y * src.width + x] = out;
            }
        }
    }

    get width() {
        return this._width;
    }

    get height() {
        return this._height;
    }

    rgba(x, y) {
        return this._rgba[y * this.width + x];
    }
}

class ImageDataSource extends Source {
    constructor(imageData) {
        super()
        this._imageData = imageData;
    }

    get width() {
        return this._imageData.width;
    }

    get height() {
        return this._imageData.height;
    }

    rgba(x, y) {
        const o = (y * this._imageData.width + x) * 4;
        return [
            this._imageData.data[o],
            this._imageData.data[o + 1],
            this._imageData.data[o + 2],
            this._imageData.data[o + 3],
        ];
    }

    static load(doc, src) {
        return new Promise((resolve, reject) => {
            const img = new Image();
            img.crossOrigin = "Anonymous";
            img.onload = () => {
                const canvas = doc.createElement("canvas");
                canvas.width = img.width;
                canvas.height = img.height;
                const ctx = canvas.getContext("2d");
                ctx.drawImage(img, 0, 0);
                const imageData =
                    ctx.getImageData(0, 0, canvas.width, canvas.height);
                resolve(new ImageDataSource(imageData));
            }
            img.onerror = (err) => reject(err);
            img.src = src;
        });
    }
}

class ParserError extends Error {
    constructor(pos, dir, msg) {
        super(`${pos.toString()},${dir}: ${msg}`);
        this.name = "ParserError";
    }
}

class Parser {
    constructor(src, pos, dir) {
        this._src = src;
        this._pos = pos ? pos : new Position(0, Math.floor(src.height / 2));
        this._dir = dir ? dir : Direction.RIGHT;
    }

    _error(msg) {
        throw new ParserError(this._pos, this._dir, msg);
    }

    _leftPixel() {
        switch (this._dir) {
            case Direction.RIGHT: return this._pos.up();
            case Direction.DOWN:  return this._pos.right();
            case Direction.LEFT:  return this._pos.down();
            case Direction.UP:    return this._pos.left();
        }
    }

    _centerPixel() {
        return this._pos;
    }

    _frontPixel() {
        switch (this._dir) {
            case Direction.RIGHT: return this._pos.right();
            case Direction.DOWN:  return this._pos.down();
            case Direction.LEFT:  return this._pos.left();
            case Direction.UP:    return this._pos.up();
        }
    }

    _rightPixel() {
        switch (this._dir) {
            case Direction.RIGHT: return this._pos.down();
            case Direction.DOWN:  return this._pos.left();
            case Direction.LEFT:  return this._pos.up();
            case Direction.UP:    return this._pos.right();
        }
    }

    parse() {
        const pattern = Pattern.parse(
            this._color(this._leftPixel()),
            this._color(this._centerPixel()),
            this._color(this._frontPixel()),
            this._color(this._rightPixel()),
        );
        switch (pattern) {
            case Pattern.ABAC:
                return new ApplicationExpression(
                    () => this._parseLeft().parse(),
                    () => this._parseFront().parse(),
                    this._pos,
                    this._dir,
                );
            case Pattern.ABCA:
                return new ApplicationExpression(
                    () => this._parseLeft().parse(),
                    () => this._parseRight().parse(),
                    this._pos,
                    this._dir,
                );
            case Pattern.ABCC:
                return new ApplicationExpression(
                    () => this._parseFront().parse(),
                    () => this._parseRight().parse(),
                    this._pos,
                    this._dir,
                );
            case Pattern.AABC:
                return new LambdaExpression(
                    this._color(this._rightPixel()),
                    () => this._parseLeft().parse(),
                    this._pos,
                    this._dir,
                );
            case Pattern.ABBC:
                return new LambdaExpression(
                    this._color(this._centerPixel()),
                    () => this._parseFront().parse(),
                    this._pos,
                    this._dir,
                );
            case Pattern.ABCB:
                return new LambdaExpression(
                    this._color(this._leftPixel()),
                    () => this._parseRight().parse(),
                    this._pos,
                    this._dir,
                );
            case Pattern.AAAB:
                return new VariableExpression(
                    this._color(this._rightPixel()),
                    this._pos,
                    this._dir,
                );
            case Pattern.AABA:
                return new VariableExpression(
                    this._color(this._frontPixel()),
                    this._pos,
                    this._dir,
                );
            case Pattern.ABAA:
                return new VariableExpression(
                    this._color(this._centerPixel()),
                    this._pos,
                    this._dir,
                );
            case Pattern.ABBB:
                return new VariableExpression(
                    this._color(this._leftPixel()),
                    this._pos,
                    this._dir,
                );
            case Pattern.ABCD:
                const left  = this._area(this._leftPixel());
                const front = this._area(this._frontPixel());
                const right = this._area(this._rightPixel());
                if (left === 1) {
                    const n = BigInt(front) ** BigInt(right);
                    return new LiteralExpression(
                        new Rational(n, 1n),
                        this._pos,
                        this._dir,
                    );
                } else if (left === 2) {
                    if (PRIMITIVES[front] && PRIMITIVES[front][right]) {
                        const primitive = PRIMITIVES[front][right];
                        return new PrimitiveExpression(
                            primitive,
                            [],
                            this._pos,
                            this._dir,
                        );
                    } else {
                        this._error(`unknown primitive: ${front}/${right}`);
                    }
                } else {
                    this._error(`Unhandled symbol: ${left}`);
                }
            case Pattern.AAAA:
                return new IdentityExpression(
                    () => this._parseFront().parse(),
                    this._pos,
                    this._dir,
                );
            case Pattern.AABB:
                return new IdentityExpression(
                    () => this._parseLeft().parse(),
                    this._pos,
                    this._dir,
                );
            case Pattern.ABAB:
                return new IdentityExpression(
                    () => this._parseRight().parse(),
                    this._pos,
                    this._dir,
                );
            case Pattern.ABBA:
                return new IdentityExpression(
                    () => this._parseFront().parse(),
                    this._pos,
                    this._dir,
                );
            default:
                this._error(`Unhandled pattern: ${pattern.toString()}`);
        }
    }

    _parseLeft() {
        return new Parser(
            this._src,
            this._leftPixel(),
            Direction.turnLeft(this._dir),
        );
    }

    _parseFront() {
        return new Parser(this._src, this._frontPixel(), this._dir);
    }

    _parseRight() {
        return new Parser(
            this._src,
            this._rightPixel(),
            Direction.turnRight(this._dir),
        );
    }

    _color(pos) {
        return this._src.hex(pos.x, pos.y);
    }

    _has(pos) {
        return pos.x >= 0 && pos.x < this._src.width &&
            pos.y >= 0 && pos.y < this._src.height;
    }

    _area(pos) {
        const visited = new Set();
        const color = this._color(pos);
        let frontier = [pos];
        while (frontier.length > 0) {
            const next = [];
            for (const p of frontier) {
                if (!visited.has(p.toString())) {
                    for (const n of p.neighbors()) {
                        if (this._has(n) && !visited.has(n.toString()) &&
                                this._color(n) === color) {
                            next.push(n);
                        }
                    }
                }
                visited.add(p.toString());
            }
            frontier = next;
        }
        return visited.size;
    }
}

class Expression {
    constructor(pos, dir) {
        this._pos = pos;
        this._dir = dir;
    }

    get position() {
        return this._pos;
    }

    get direction() {
        return this._dir;
    }

    async whnf(ctx) {
        await ctx.onWhnf(this);
        return this;
    }

    async apply(ctx, arg) {
        return new ApplicationExpression(
            () => this,
            () => arg,
            this.position,
            this.direction,
        );
    }

    freeVars() {
        return new Set();
    }

    allVars() {
        return new Set();
    }

    subst(x, s) {
        return this;
    }

    value() {
        return null;
    }
}

class ApplicationExpression extends Expression {
    constructor(lhsf, rhsf, pos, dir) {
        super(pos, dir);
        this._lhsf = lhsf;
        this._rhsf = rhsf;
    }

    get lhs() {
        if (!this._lhs) {
            this._lhs = this._lhsf();
        }
        return this._lhs;
    }

    get rhs() {
        if (!this._rhs) {
            this._rhs = this._rhsf();
        }
        return this._rhs;
    }

    toString() {
        const lhs = this.lhs.toString();
        const rhs = this.rhs.toString();
        return `(${lhs} ${rhs})`;
    }

    async whnf(ctx) {
        await ctx.onWhnf(this);
        const lhs = await this.lhs.whnf(ctx);
        return lhs.apply(ctx, this.rhs);
    }

    freeVars() {
        return this.lhs.freeVars().union(this.rhs.freeVars());
    }

    allVars() {
        return this.lhs.allVars().union(this.rhs.allVars());
    }

    subst(x, s) {
        return new ApplicationExpression(
            () => this.lhs.subst(x, s),
            () => this.rhs.subst(x, s),
            this.position,
            this.direction,
        );
    }
}

class LambdaExpression extends Expression {
    constructor(variable, bodyf, pos, dir) {
        super(pos, dir);
        this._variable = variable;
        this._bodyf = bodyf;
    }

    get body() {
        if (!this._body) {
            this._body = this._bodyf()
        }
        return this._body;
    }

    toString() {
        const body = this.body.toString();
        return `(\\${this._variable} -> ${body})`;
    }

    async apply(ctx, arg) {
        return this.body.subst(this._variable, arg).whnf(ctx);
    }

    freeVars() {
        const fvs = this.body.freeVars();
        fvs.delete(this._variable);
        return fvs;
    }

    allVars() {
        const avs = this.body.allVars();
        avs.add(this._variable);
        return avs;
    }

    subst(x, s) {
        if (x === this._variable) {
            // This lambda binds more tightly, no need to change
            return this;
        }

        const fvs = s.freeVars();
        if (fvs.has(this._variable)) {
            // Avoid capturing `this._variable`
            const body = this.body;
            const avs = fvs.union(body.allVars());
            let fresh = 0;
            while (avs.has(`fresh_${fresh}`)) {
                fresh++
            }
            const variable = `fresh_${fresh}`;
            return new LambdaExpression(
                variable,
                () => body.
                    subst(this._variable, new VariableExpression(variable)).
                    subst(x, s),
                this.position,
                this.direction,
            )
        }

        return new LambdaExpression(
            // Continue substitution
            this._variable,
            () => this.body.subst(x, s),
            this.position,
            this.direction,
        );
    }
}

class VariableExpression extends Expression {
    constructor(variable, pos, dir) {
        super(pos, dir);
        this._variable = variable;
    }

    toString() {
        return this._variable;
    }

    freeVars() {
        return new Set([this._variable]);
    }

    allVars() {
        return new Set([this._variable]);
    }

    subst(x, e) {
        if (x === this._variable) {
            return e;
        } else {
            return this;
        }
    }
}

class PrimitiveExpression extends Expression {
    constructor(primitive, args, pos, dir) {
        super(pos, dir);
        this._primitive = primitive;
        this._args = args ? args : [];
    }

    toString() {
        if (this._args.length === 0) {
            return this._primitive.name;
        } else {
            return this._primitive.name + "(" +
                this._args.map((a) => a.toString()).join(", ") + ")";
        }
    }

    async apply(ctx, arg) {
        const args = [...this._args, arg]
        if (args.length === this._primitive.arity) {
            return this._primitive.implementation(ctx, args);
        } else {
            return new PrimitiveExpression(
                this._primitive,
                args,
                this.position,
                this.direction
            );
        }
    }
}

class LiteralExpression extends Expression {
    constructor(value, pos, dir) {
        super(pos, dir);
        this._value = value;
    }

    toString() {
        return this._value.toString();
    }

    value() {
        return this._value;
    }
}

class IdentityExpression extends Expression {
    constructor(exprf, pos, dir) {
        super(pos, dir);
        this._exprf = exprf;
    }

    get expr() {
        if (!this._expr) {
            this._expr = this._exprf()
        }
        return this._expr;
    }

    toString() {
        return this.expr.toString();
    }

    async whnf(ctx) {
        await ctx.onWhnf(this);
        return await this.expr.whnf(ctx);
    }

    freeVars() {
        return this.expr.freeVars();
    }

    allVars() {
        return this.expr.freeVars();
    }

    subst(x, s) {
        return new IdentityExpression(
            () => this.expr.subst(x, s),
            this.position,
            this.direction,
        );
    }
}

const PRIMITIVES = {
    1: {
        1: {
            name: "in_num",
            arity: 2,
            implementation: async (ctx, args) => {
                const input = await ctx.inputNumber();
                const rational = new Rational(BigInt(input), 1n);
                const lit = new LiteralExpression(rational);
                const applied = await args[0].apply(ctx, lit);
                return applied.whnf(ctx);
            },
        },
    },
    2: {
        1: {
            name: "out_num",
            arity: 2,
            implementation: async (ctx, args) => {
                const lhs = await args[0].whnf(ctx);
                const out = lhs.value();
                console.log(out);
                await ctx.outputNumber(out.toString());
                return args[1].whnf(ctx);
            },
        },
    },
    3: {
        1: {
            name: "num_add",
            arity: 2,
            implementation: async (ctx, args) => {
                const lhs = await args[0].whnf(ctx);
                const rhs = await args[1].whnf(ctx);
                return new LiteralExpression(lhs.value().add(rhs.value()));
            },
        },
        2: {
            name: "num_sub",
            arity: 2,
            implementation: async (ctx, args) => {
                const lhs = await args[0].whnf(ctx);
                const rhs = await args[1].whnf(ctx);
                return new LiteralExpression(lhs.value().subtract(rhs.value()));
            },
        },
        3: {
            name: "num_mul",
            arity: 2,
            implementation: async (ctx, args) => {
                const lhs = await args[0].whnf(ctx);
                const rhs = await args[1].whnf(ctx);
                return new LiteralExpression(lhs.value().multiply(rhs.value()));
            },
        },
        4: {
            name: "num_div",
            arity: 2,
            implementation: async (ctx, args) => {
                const lhs = await args[0].whnf(ctx);
                const rhs = await args[1].whnf(ctx);
                return new LiteralExpression(lhs.value().divide(rhs.value()));
            },
        },
    },
};

class AnnotatedView {
    constructor(doc, src) {
        this._doc = doc;
        this._ns = "http://www.w3.org/2000/svg";
        this._src = src;
        this._factor = 20;
        this._padding = 0.1;
        this._focus = null;
        this._svg = doc.createElementNS(this._ns, "svg");
        const width = src.width + this._padding * 2;
        const height = src.height + this._padding * 2;
        this._svg.setAttribute("width", this._factor * width);
        this._svg.setAttribute("height", this._factor * height);
        this._svg.setAttribute(
            "viewBox",
            `-${this._padding} -${this._padding} ${width} ${height}`,
        );

        for (let y = 0; y < this._src.height; y++) {
            for (let x = 0; x < this._src.width; x++) {
                const alpha = this._src.rgba(x, y)[3];
                if (alpha > 0) {
                    const rect = this._doc.createElementNS(this._ns, "rect");
                    rect.setAttribute("x", x);
                    rect.setAttribute("y", y);
                    rect.setAttribute("width", "1");
                    rect.setAttribute("height", "1");
                    rect.setAttribute("fill", this._src.hex(x, y));
                    rect.setAttribute("stroke", "none");
                    this._svg.appendChild(rect);
                }
            }
        }
    }

    get svg() {
        return this._svg;
    }

    focus(pos, dir) {
        if (!pos || !dir) {
            return;
        }

        if (this._focus) {
            this._svg.removeChild(this._focus);
        }

        const points = [
            [0, -1],
            [1, -1],
            [1, 0],
            [2, 0],
            [2, 1],
            [1, 1],
            [1, 2],
            [0, 2],
            [0, -1],
        ].map(([x, y]) => {
            switch (dir) {
                case Direction.RIGHT: return [x, y];
                case Direction.DOWN:  return [y, x];
                case Direction.LEFT:  return [1 - x, y];
                case Direction.UP:    return [1 - y, 1 - x];
            }
        }).map((xy) => xy.join(",")).join(" ");

        const rect = this._doc.createElementNS(this._ns, "polyline");
        rect.setAttribute("fill", "none");
        rect.setAttribute("stroke", "black");
        rect.setAttribute("stroke-width", "0.1");
        rect.setAttribute("transform", `translate(${pos.x} ${pos.y})`);
        rect.setAttribute("points", points);
        this._focus = rect;
        this._svg.appendChild(this._focus);
    }
}

const runInterpreter = async (doc, element, src) => {
    let view;
    let paused = null;
    let output = () => {};

    const div = doc.createElement("div");
    div.setAttribute("class", "interpreter");

    const evalCtx = {
        inputNumber: () => {
            return new Promise((resolve, reject) => {
                const form = doc.createElement("form");
                const input = doc.createElement("input");
                input.type = "text";
                form.appendChild(input);
                const submit = doc.createElement("button");
                submit.type = "submit";
                submit.textContent = "submit";
                form.appendChild(submit);
                div.appendChild(form);

                form.addEventListener("submit", (event) => {
                    event.preventDefault();
                    const value = input.value;
                    if (value) {
                        div.removeChild(form);
                        resolve(Number(value));
                    } else {
                        reject("Input is empty");
                    }
                });
            });
        },
        outputNumber: (num) => {
            output(num.toString());
        },
        onWhnf: async (expr) => {
            if (paused) {
                await paused;
            }
            view.focus(expr.position, expr.direction);
            await new Promise(r => setTimeout(r, 50));
        }
    }

    const source = new DenoiseSource(await ImageDataSource.load(doc, src));
    view = new AnnotatedView(doc, source);
    div.appendChild(view.svg);

    let unpause = () => {};
    view.svg.onclick = (event) => {
        event.preventDefault();
        if (!paused) {
            paused = new Promise((resolve, reject) => {
                unpause = () => {
                    resolve();
                    paused = null;
                };
            });
        } else {
            unpause();
        }
    };

    const pre = doc.createElement("pre");
    pre.setAttribute("class", "output");
    const code = doc.createElement("code");
    output = (line) => {
        code.appendChild(new Text(line + "\n"));
        pre.scrollTo(0, pre.scrollHeight);
    };
    pre.appendChild(code);
    div.appendChild(pre);

    element.replaceWith(div);

    output("Starting interpreter...");
    const parser = new Parser(source);
    try {
        const whnf = await parser.parse().whnf(evalCtx);
        if (whnf.value() === null) {
            output("Interpreter exited with expression:");
            output(whnf.toString());
        } else {
            output(`Interpreter exited with code ${whnf.value()}`);
        }
    } catch(e) {
        output(`Interpreter crashed: ${e}`);
    }
};
