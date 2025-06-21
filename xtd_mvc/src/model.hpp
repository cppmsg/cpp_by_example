#pragma once
#include <xtd/event>
#include <xtd/event_handler>
// Model Data: holds & update_fn for live/true data.
// also Event queue??: holds Event queue/loop?? with callbackfn handlers?? TODO??: why?
class Model : public xtd::object {  // Instanciated/Held by View.
    int value_{0};  // Data in Model
public:
    xtd::event<Model, xtd::event_handler>
        event_value_changed_cMy_ModelaHandler_{};

    int  value() const {
        return value_;
    }
    void model_increment_counter() {  // Called by Controller only
        ++value_;
        this->event_value_changed_cMy_ModelaHandler_ (
            *this,                    // this whole Model class, not just this class fn. ?? TODO??:
            xtd::event_args::empty ); // how about not empty, but one or two? TODO??:
    }
};
