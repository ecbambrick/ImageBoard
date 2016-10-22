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
    register: ({ action, shortcut, trigger, enabled = () => true }) => {

        // Handle keyboard shortcut.
        if (shortcut) {
            const eventType = shortcut.event ? shortcut.event
                                             : "keyup";

            window.addEventListener(eventType, (e) => {
                const inputFocused  = shortcut.allowInInput || Action.isFreeFocus();
                const shortcutValid = Action._checkShortcut(e, shortcut);

                if (enabled() && inputFocused && shortcutValid) {
                    action();
                }
            });
        }

        // Handle object event.
        if (trigger) {
            while (trigger.length >= 2) {
                const object    = trigger[0];
                const eventName = trigger[1];
                trigger.splice(0, 2);

                object.addEventListener(eventName, (e) => {
                    if (enabled()) {
                        action();
                    }

                    e.preventDefault();
                });
            }
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
            && Action._keyMapping(shortcut.key) === e.keyCode;
    },

    // The mapping from a key name (i.e. "escape") to a key code (i.e. 27).
    _keyMapping: (key) => {
        if      (key == "enter")  return 13;
        else if (key == "escape") return 27;
        else if (key == "space")  return 32;
        else if (key == "delete") return 46;
        else                      return key.toUpperCase().charCodeAt(0);
    }
}
