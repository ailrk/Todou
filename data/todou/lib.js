/*
 * YMD
 */
;
export function fmtYM(ymd) {
    return `${ymd.year}-${String(ymd.month + 1).padStart(2, '0')}`;
}
export function fmtYMD(ymd) {
    let d = ymd.day ? ymd.day : 1;
    return `${ymd.year}-${String(ymd.month + 1).padStart(2, '0')}-${String(d).padStart(2, '0')}`;
}
export function getDateFromPath(defaultDate = new Date()) {
    const match = window.location.pathname.match(/(\d{4}-\d{2}-\d{2})/);
    if (!match)
        return defaultDate;
    const [year, month, day] = match[1].split("-").map(Number);
    const date = new Date(year, month - 1, day);
    return isNaN(date.getTime()) ? defaultDate : date;
}
export function dateToLocalISOString(date) {
    return date.toLocaleString('sv').replace(' ', 'T');
}
/*
 * Presence View
 */
// nBits per segment. A segment holds all the flags for a day.
// This number depends on the presence map format from the backend.
const NBITS = 2 | 0;
// Number of segments per byte
const NSEGS = 8 / NBITS | 0;
const PRESENCE_DAY = 0;
const PRESENCE_COMPLETED = 1;
export class PresenceView {
    bytes;
    constructor(input) {
        // PresenceView takes the owndership of the buffer.
        // If we get a view, we copy the view into a new buffer. Otherwise we simply
        // take it.
        if (ArrayBuffer.isView(input)) {
            this.bytes = new Uint8Array(input.buffer, input.byteOffset, input.byteLength);
        }
        else {
            this.bytes = new Uint8Array(input);
        }
    }
    // @i: the number of segment. 0 indexed, represents day
    seg(i) {
        if (NBITS < 0 && (NBITS & NBITS - 1) !== 0) {
            console.error("seg size must be positive and power of 2");
        }
        const mask = (1 << NBITS) - 1;
        return (this.bytes[Math.floor(i / NSEGS)] >> (NBITS * (i % NSEGS))) & mask;
    }
    hasDay(i) {
        return (this.seg(i) >> PRESENCE_DAY) & 1;
    }
    completed(i) {
        return (this.seg(i) >> PRESENCE_COMPLETED) & 1;
    }
    view(i, calendar, firstDay) {
        let result = { presence: false, completed: false };
        let cday = new Date(fmtYMD(calendar) + "T00:00:00");
        let fday = new Date(firstDay + "T00:00:00"); // use local time
        cday.setDate(i);
        cday.setHours(0, 0, 0, 0);
        fday.setHours(0, 0, 0, 0);
        let diff = cday.getTime() - fday.getTime();
        if (Number.isNaN(diff)) {
            return result;
        }
        let delta = Math.round(diff / (1000 * 60 * 60 * 24));
        if (this.hasDay(delta)) {
            result.presence = true;
        }
        if (this.completed(delta)) {
            result.completed = true;
        }
        return result;
    }
    dump() {
        function byteToHex(byte) {
            // Ensure the byte value is treated as an unsigned 8-bit integer (0-255)
            // which is useful if the input might be a signed number.
            const unsignedByte = byte & 0xFF;
            // Convert the number to a base-16 string.
            const hex = unsignedByte.toString(16);
            // Pad with a leading '0' and take the last two characters to ensure
            // a consistent 2-character output (e.g., 5 becomes "05").
            return hex.padStart(2, '0');
        }
        this.bytes.forEach(b => console.log(byteToHex(b)));
    }
}
export async function base64ToBitSet(b64) {
    const blob = await fetch(`data:application/octet-stream;base64,${b64}`).then(r => r.blob());
    const bytes = await zlibDecompress(blob);
    return new PresenceView(bytes);
}
export async function zlibDecompress(blob) {
    const ds = new DecompressionStream('deflate');
    const stream = blob.stream().pipeThrough(ds);
    const response = await new Response(stream).arrayBuffer();
    return new Uint8Array(response);
}
