#pragma once
#include "counter_model.hpp"

class counter_controller {
public:
  explicit counter_controller(counter_model& model) : model_(model) {}
  
  void increment_counter() {
    model_.increment();
  }
  
private:
  counter_model& model_;
};
