#pragma once
#include "model.hpp"
// Controller only calls data updates on Model.
// Instanciated/Held by View with Model is passed local use to find update functions on Model.
class Controller {
    Model& model_ref_;  // ref points to Model, but not View?
public:
    explicit  //CONSTRUCTOR receives Model& from View_main_form had Instanciated & molds Model
    Controller(Model& model_ref):model_ref_{model_ref}{}

    // *** vvv Called by button.click by event loop???
    void controller_increment_counter() { model_ref_.model_increment_counter(); }
};
