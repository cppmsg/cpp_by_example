#pragma once
#include "model.hpp"
#include "controller.hpp"
#include <xtd/forms/button>
#include <xtd/forms/label>
#include <xtd/forms/form>
class View_main_form : public xtd::forms::form {
    xtd::forms::button  button_{};
    xtd::forms::label   label_{};
    Model               model_{};
    Controller          controller_{model_}; // Controller controller_=Controller(model_);
public:
    View_main_form() {  // CONSTRUCTOR
        this->text("MVC Example");
        this->controls().push_back_range({button_, label_});  // TODO??: place after the setters below?
        button_.location({10, 10});
        button_.text("Incrementer");
        button_.click += [&] { controller_.controller_increment_counter(); };
        label_.location({10, 50});
        label_.auto_size(true);
        // vvv Subscribe/+= *this_view AND local update_view callback_fn when ??? model changed events/updates
        model_.event_value_changed_cMy_ModelaHandler_+=
            { *this,
              & View_main_form::on_model_value_changed_callback };
        this->update_view_data();
        // *** Now it Keeps Running within main():::run() based on UI View events ***  a Single EVENT LOOP ??
    }
private:
    void update_view_data() {
        label_.text( xtd::ustring::format( "Actual value: {}", model_.value() ) );
    }
    void on_model_value_changed_callback(xtd::object& senderUU, const xtd::event_args & event_argsUU) {
        this->update_view_data();
    }
};
