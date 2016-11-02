"use strict"

// -----------------------------------------------------------------------------
// HTML Collection
// -----------------------------------------------------------------------------

HTMLCollection.prototype.forEach = function(f) {
    return [].forEach.call(this, f);
}

// -----------------------------------------------------------------------------
// HTML Collection
// -----------------------------------------------------------------------------

Array.prototype.last = function() {
    return this[this.length-1];
}

// -----------------------------------------------------------------------------
// HTML Collection
// -----------------------------------------------------------------------------

KeyboardEvent.prototype.keyName = function() {
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
        .filter(x => x.keyName() == key
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
