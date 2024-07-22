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
    RIGHT: Symbol("RIGHT"),
    DOWN: Symbol("DOWN"),
    LEFT: Symbol("LEFT"),
    UP: Symbol("UP"),
};

const turnLeft = (dir) => {
    switch (dir) {
        case Direction.RIGHT: return Direction.UP;
        case Direction.DOWN:  return Direction.RIGHT;
        case Direction.LEFT:  return Direction.DOWN;
        case Direction.UP:    return Direction.LEFT;
    }
};

const turnRight = (dir) => {
    switch (dir) {
        case Direction.RIGHT: return Direction.DOWN;
        case Direction.DOWN:  return Direction.LEFT;
        case Direction.LEFT:  return Direction.UP;
        case Direction.UP:    return Direction.RIGHT;
    }
};

const Pattern = {
    AAAA: Symbol("AAAA"),
    AAAB: Symbol("AAAB"),
    AABA: Symbol("AABA"),
    AABB: Symbol("AABB"),
    AABC: Symbol("AABC"),
    ABAA: Symbol("ABAA"),
    ABAB: Symbol("ABAB"),
    ABAC: Symbol("ABAC"),
    ABBA: Symbol("ABBA"),
    ABBB: Symbol("ABBB"),
    ABBC: Symbol("ABBC"),
    ABCA: Symbol("ABCA"),
    ABCB: Symbol("ABCB"),
    ABCC: Symbol("ABCC"),
    ABCD: Symbol("ABCD"),
};

const parsePattern = (a, x, y, z) => {
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
};

class ImageDataSource {
    constructor(imageData) {
        this._imageData = imageData;
    }

    get width() {
        return this._imageData.width;
    }

    get height() {
        return this._imageData.width;
    }

    color(pos) {
        const o = (pos.y * this._imageData.width + pos.x) * 4;
        const hr = this._imageData.data[o    ].toString(16).padStart(2, "0");
        const hg = this._imageData.data[o + 1].toString(16).padStart(2, "0");
        const hb = this._imageData.data[o + 2].toString(16).padStart(2, "0");
        const ha = this._imageData.data[o + 3].toString(16).padStart(2, "0");
        return `#${hr}${hg}${hb}${ha}`;
    }
}

class Parser {
    constructor(src, pos, dir) {
        this._src = src;
        this._pos = pos ? pos : new Position(0, Math.floor(src.height / 2));
        this._dir = dir ? dir : Direction.RIGHT;
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
            case Direction.RIGHT:
                return this._pos.down();
            case Direction.DOWN:
                return this._pos.left();
            case Direction.LEFT:
                return this._pos.up();
            case Direction.UP:
                return this._pos.right();
        }
    }

    parse() {
        const pattern = parsePattern(
            this._src.color(this._leftPixel()),
            this._src.color(this._centerPixel()),
            this._src.color(this._frontPixel()),
            this._src.color(this._rightPixel()),
        );
        switch (pattern) {
            case Pattern.ABAC:
                return new ApplicationExpression(
                    () => this._parseLeft().parse(),
                    () => this._parseFront().parse(),
                );
            case Pattern.ABCA:
                return new ApplicationExpression(
                    () => this._parseLeft().parse(),
                    () => this._parseRight().parse(),
                );
            case Pattern.ABCC:
                return new ApplicationExpression(
                    () => this._parseFront().parse(),
                    () => this._parseRight().parse(),
                );
            case Pattern.ABBC:
                return new LambdaExpression(
                    this._color(this._centerPixel()),
                    () => this._parseFront().parse(),
                );
            case Pattern.AABA:
                return new VariableExpression(this._color(this._frontPixel()));
            case Pattern.ABAA:
                return new VariableExpression(this._color(this._centerPixel()));
            case Pattern.ABCD:
                const left  = this._area(this._leftPixel());
                const front = this._area(this._frontPixel());
                const right = this._area(this._rightPixel());
                if (left === 1) {
                    const n = BigInt(front) ** BigInt(right);
                    return new LiteralExpression(new Rational(n, 1n));
                } else if (left === 2) {
                    if (PRIMITIVES[front] && PRIMITIVES[front][right]) {
                        const primitive = PRIMITIVES[front][right];
                        return new PrimitiveExpression(primitive);
                    } else {
                        throw new Error(`Unknown primitive: ${front}/${right}`);
                    }
                } else {
                    throw new Error(`Unhandled symbol: ${left}`);
                }
            case Pattern.AAAA:
                return new IdentityExpression(() => this._parseFront().parse());
            case Pattern.AABB:
                return new IdentityExpression(() => this._parseLeft().parse());
            case Pattern.ABAB:
                return new IdentityExpression(() => this._parseRight().parse());
            case Pattern.ABBA:
                return new IdentityExpression(() => this._parseFront().parse());
            default:
                throw new Error(`Unhandled pattern: ${pattern.toString()}`);
        }
    }

    _parseLeft() {
        return new Parser(this._src, this._leftPixel(), turnLeft(this._dir));
    }

    _parseFront() {
        return new Parser(this._src, this._frontPixel(), this._dir);
    }

    _parseRight() {
        return new Parser(this._src, this._rightPixel(), turnRight(this._dir));
    }

    _has(pos) {
        return pos.x >= 0 && pos.x < this._src.width &&
            pos.y >= 0 && pos.y < this._src.height;
    }

    _area(pos) {
        const visited = new Set();
        const color = this._src.color(pos);
        let frontier = [pos];
        while (frontier.length > 0) {
            const next = [];
            for (const p of frontier) {
                if (!visited.has(p.toString())) {
                    for (const n of p.neighbors()) {
                        if (this._has(n) && !visited.has(n.toString()) &&
                                this._src.color(n) === color) {
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
    whnf() {
        return this;
    }

    apply(arg) {
        return new ApplicationExpression(() => this, () => arg);
    }

    value() {
        return null;
    }
}

class ApplicationExpression extends Expression {
    constructor(lhs, rhs) {
        super()
        this._lhs = lhs;
        this._rhs = rhs;
    }

    toString() {
        const lhs = this._lhs().toString();
        const rhs = this._rhs().toString();
        return `(${lhs} ${rhs})`;
    }
}

class LambdaExpression extends Expression {
    constructor(variable, body) {
        super()
        this._variable = variable;
        this._body = body;
    }

    toString() {
        const body = this._body().toString();
        return `(\\${this._variable} -> ${body})`;
    }
}

class VariableExpression extends Expression {
    constructor(variable) {
        super()
        this._variable = variable;
    }

    toString() {
        return this._variable;
    }
}

class PrimitiveExpression extends Expression {
    constructor(primitive, args) {
        super()
        this._primitive = primitive;
        this._args = args ? args : [];
    }

    toString() {
        return this._primitive.name;
    }
}

class LiteralExpression extends Expression {
    constructor(value) {
        super()
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
    constructor(expr) {
        super()
        this._expr = expr;
    }

    toString() {
        return this._expr().toString();
    }
}

const PRIMITIVES = {
    3: {
        2: {
            name: "num_sub",
            arity: 2,
            implementation: (args) => {
                const lhs = args[0].whnf().value();
                const rhs = args[1].whnf().value();
                return new LiteralExpression(lhs.subtract(rhs));
            }
        }
    }
};
