module.exports = class Encryption {
    trunc(x) {
        if (x > 255) {
            const counts = Math.trunc(x / 256);
            x -= counts * 256;
        } else if (x < 0) {
            x += 256;
        }

        return x;
    }

    sub(x, y) {
        y = this.trunc(y);

        x -= y;

        return this.trunc(x);
    }

    add(x, y) {
        y = this.trunc(y);

        x += y;

        return this.trunc(x);
    }

    enc(buffer) {
        const buf = [];

        for (let i = 0; i < buffer.length; i++) {
            const count = Math.trunc(i / 256);

            buf.push(
                count % 2 === 0 ?
                this.sub(buffer[i], i) :
                this.add(buffer[i], i)
            );
        }

        return Buffer.from(buf);
    }

    dec(buffer) {
        const buf = [];

        for (let i = 0; i < buffer.length; i++) {
            const count = Math.trunc(i / 256);

            buf.push(
                count % 2 === 1 ?
                this.sub(buffer[i], i) :
                this.add(buffer[i], i)
            );
        }

        return Buffer.from(buf);
    }
};
