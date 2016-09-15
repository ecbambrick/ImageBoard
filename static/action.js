"use strict"

// Functions for registering events and keyboard shortcuts.
const Action = {

    // Registers the given action as an event.
    // action = {
    //     shortcut: {
    //         key:          string,
    //         shift:        bool,
    //         ctrl:         bool,
    //         alt:          bool,
    //         allowInInput: bool,
    //         event:        string
    //     }
    //     trigger: [ object, string ]
    //     enabled: bool function()
    //     action:  void function()
    // }
    register: (options) => {
        const action  = options.action;
        const enabled = options.enabled !== undefined ? options.enabled
                                                      : () => true
        // Handle keyboard shortcut.
        if (options.shortcut !== undefined) {
            const shortcut  = options.shortcut;
            const eventType = shortcut.event !== undefined ? shortcut.event
                                                           : "keyup";
            window.addEventListener(eventType, (e) => {
                if (enabled()
                && (shortcut.allowInInput || Action.isFreeFocus())
                && Action._checkShortcut(e, shortcut)) {
                    action();
                }
            });
        }

        // Handle object event.
        if (options.trigger !== undefined) {
            const object    = options.trigger[0];
            const eventName = options.trigger[1];

            object.addEventListener(eventName, (e) => {
                if (enabled()) {
                    action();
                }
            });
        }
    },

    // Checks whether or not the document is currently focused on any input
    // element such as a text box or video.
    isFreeFocus: () => {
        const active = document.activeElement;

        return active.type    != "text"
            && active.type    != "textarea"
            && active.tagName != "VIDEO"
            && active.tagName != "BUTTON";
    },

    // Checks the given key event against the given shortcut to see if it
    // matches.
    _checkShortcut: (e, shortcut) => {
        return !(shortcut.shift ^ e.shiftKey)
            && !(shortcut.ctrl  ^ e.ctrlKey)
            && !(shortcut.alt   ^ e.altKey)
            && Action._keyMapping.get(shortcut.key) === e.keyCode;
    },

    // The mapping from a key name (i.e. "escape") to a key code (i.e. 27).
    _keyMapping: new Map()
        .set("enter",   13)
        .set("escape",  27)
        .set("space",   32)
        .set("delete",  46)
        .set("e",       69)
        .set("p",       80)
        .set("q",       81)
        .set("s",       83)
}
