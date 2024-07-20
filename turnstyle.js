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

    sub(other) {
        return new Rational(
            this._num * other._denom - other._num * this._denom,
            this._denom * other._denom,
        );
    }

    mul(other) {
        return new Rational(this._num * other._num, this._denom * other._denom);
    }

    div(other) {
        return new Rational(this._num * other._denom, this._denom * other._num);
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

const r = new Rational(3n, 2n);
const o = new Rational(5n, 6n);
console.log(r.add(o).sub(new Rational(2n, 6n)));
