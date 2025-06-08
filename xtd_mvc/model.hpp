#pragma once
#include <xtd/event_args>       // grostig
#include <xtd/event_handler>    // grostig
#include <xtd/event>

class counter_model {
public:
    int value() const {return value_;}

    void increment() {
        ++value_;
        value_changed(*this, xtd::event_args::empty);
    }

    xtd::event<counter_model, xtd::event_handler> value_changed;

private:
    int value_ = 0;
};
