"use strict"

// -----------------------------------------------------------------------------
// Node List
// -----------------------------------------------------------------------------

NodeList.prototype.isEmpty = function() {
    return this.length == 0;
}

// -----------------------------------------------------------------------------
// HTML Collection
// -----------------------------------------------------------------------------

HTMLCollection.prototype.forEach = function(f) {
    return [].forEach.call(this, f);
}

HTMLCollection.prototype.slice = function(start, end) {
    return [].slice.call(this, start, end);
}

// -----------------------------------------------------------------------------
// Array
// -----------------------------------------------------------------------------

Array.prototype.last = function() {
    return this[this.length-1];
}

Array.prototype.exceptLast = function() {
    return this.slice(0, this.length-1);
}

// -----------------------------------------------------------------------------
// Keyboard Event
// -----------------------------------------------------------------------------

KeyboardEvent.prototype.friendlyKey = function() {
    if (this.key === " ") return "space"
    else                  return this.key.toLowerCase();
}

// -----------------------------------------------------------------------------
// Kefir
// -----------------------------------------------------------------------------

Kefir.fromKey = (binding, { source, allowInInput } = {}) => {
    if (typeof binding !== "string") {
        throw "Key binding must be a string (i.e. 'shift+x')."
    }

    source          = source ? source : document
    const modifiers = binding.split(/\s*\+\s*/);
    const shift     = modifiers.includes("shift");
    const ctrl      = modifiers.includes("ctrl");
    const alt       = modifiers.includes("alt");
    const key       = modifiers.last();

    return Kefir
        .fromEvents(source, 'keyup')
        .filter(x => x.friendlyKey() == key
                  && !(x.shiftKey ^ shift)
                  && !(x.altKey   ^ alt)
                  && !(x.ctrlKey  ^ ctrl)
                  &&  (allowInInput || Action.isFreeFocus()));
}

Kefir.fromClick = (x) => {
    return Kefir
        .fromEvents(x, "click", e => {
            e.preventDefault();
            return e;
        })
}
