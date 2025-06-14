#pragma once
#include "model.hpp"

class Controller {  // Instanciated/Held by View, Model is passed.
    Model& model_ref_;  // ref points to Model, but not View?
public:
    explicit
    Controller(Model& model):model_ref_{model}{} // Constructor recieves ref to Model from event which Instanciate/Holds Model

    void increment_counter() { model_ref_.increment_counter(); }
};
