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

    up() {
        return new Position(this.x, this.y - 1);
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

const turnRight = (dir) => {
    switch (dir) {
        case Direction.RIGHT:
            return Direction.DOWN;
        case Direction.DOWN:
            return Direction.LEFT;
        case Direction.LEFT:
            return Direction.UP;
        case Direction.UP:
            return Direction.RIGHT;
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

class Parser {
    constructor(imageData, pos, dir) {
        this._imageData = imageData;
        const height = this._imageData.height;
        this._pos = pos ? pos : new Position(0, Math.floor(height / 2));
        this._dir = dir ? dir : Direction.RIGHT;
    }

    _leftPixel() {
        switch (this._dir) {
            case Direction.RIGHT:
                return this._pos.up();
            case Direction.DOWN:
                return this._pos.right();
            case Direction.LEFT:
                return this._pos.down();
            case Direction.UP:
                return this._pos.left();
        }
    }

    _centerPixel() {
        return this._pos;
    }

    _frontPixel() {
        switch (this._dir) {
            case Direction.RIGHT:
                return this._pos.right();
            case Direction.DOWN:
                return this._pos.down();
            case Direction.LEFT:
                return this._pos.left();
            case Direction.UP:
                return this._pos.up();
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
            this._color(this._leftPixel()),
            this._color(this._centerPixel()),
            this._color(this._frontPixel()),
            this._color(this._rightPixel()),
        );
        switch (pattern) {
            case Pattern.ABCC:
                return new ApplicationExpression(
                    this._frontParser(),
                    this._rightParser(),
                );
            case Pattern.ABBA:
                return new IdentityExpression(this._frontParser());
            default:
                throw new Error(`Unhandled pattern: ${pattern.toString()}`);
        }
    }

    _frontParser() {
        return new Parser(this._imageData, this._frontPixel(), this._dir);
    }

    _color(pos) {
        const o = (pos.y * this._imageData.width + pos.x) * 4;
        const hr = this._imageData.data[o    ].toString(16).padStart(2, "0");
        const hg = this._imageData.data[o + 1].toString(16).padStart(2, "0");
        const hb = this._imageData.data[o + 2].toString(16).padStart(2, "0");
        const ha = this._imageData.data[o + 3].toString(16).padStart(2, "0");
        return `#${hr}${hg}${hb}${ha}`;
    }
}

class IdentityExpression {
    constructor(parser) {
        this._parser = parser;
    }

    eval() {
        return this._parser.parse().eval();
    }
}
