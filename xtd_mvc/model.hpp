#pragma once
#include <xtd/event>
#include <xtd/event_handler>

class Model : public xtd::object {  // Instanciated/Held by View.
    int                                   value_{0};
public:
    xtd::event<Model, xtd::event_handler>
        event_value_changed_cMy_ModelaHandler_{};

    int  value() const {
        return value_;
    }
    void increment_counter() {
        ++value_;
        this->event_value_changed_cMy_ModelaHandler_ (
            *this,
            xtd::event_args::empty ); // !empty, one or two? TODO??:
    }
};
