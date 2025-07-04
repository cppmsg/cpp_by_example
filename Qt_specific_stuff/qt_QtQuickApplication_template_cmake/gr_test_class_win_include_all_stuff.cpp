#include "gr_test_class_win_include_all_stuff.hpp"

#include <utility>

class gr_test_class_win_include_all_stuffData : public QSharedData {
public:
};

gr_test_class_win_include_all_stuff::gr_test_class_win_include_all_stuff(QWidget *parent)
    : QMainWindow{ parent }, data(new gr_test_class_win_include_all_stuffData) {}

gr_test_class_win_include_all_stuff::gr_test_class_win_include_all_stuff(const gr_test_class_win_include_all_stuff &rhs)
    : data{ rhs.data } {}

gr_test_class_win_include_all_stuff::gr_test_class_win_include_all_stuff(gr_test_class_win_include_all_stuff &&rhs)
    : data{ std::move(rhs.data) } {}

gr_test_class_win_include_all_stuff &
gr_test_class_win_include_all_stuff::operator=(const gr_test_class_win_include_all_stuff &rhs) {
    if(this != &rhs) data = rhs.data;
    return *this;
}

gr_test_class_win_include_all_stuff &
gr_test_class_win_include_all_stuff::operator=(gr_test_class_win_include_all_stuff &&rhs) {
    if(this != &rhs) data = std::move(rhs.data);
    return *this;
}

gr_test_class_win_include_all_stuff::~gr_test_class_win_include_all_stuff() {}
