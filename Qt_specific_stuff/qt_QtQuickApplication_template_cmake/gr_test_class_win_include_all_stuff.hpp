#pragma once

#include <QMainWindow>
#include <QObject>
#include <QQuickItem>
#include <QSharedDataPointer>
#include <QWidget>

class gr_test_class_win_include_all_stuffData;

class gr_test_class_win_include_all_stuff : public QMainWindow {
    Q_OBJECT
    QML_ELEMENT
public:
    explicit gr_test_class_win_include_all_stuff(QWidget *parent = nullptr);
    gr_test_class_win_include_all_stuff(const gr_test_class_win_include_all_stuff &);
    gr_test_class_win_include_all_stuff(gr_test_class_win_include_all_stuff &&);
    gr_test_class_win_include_all_stuff &operator=(const gr_test_class_win_include_all_stuff &);
    gr_test_class_win_include_all_stuff &operator=(gr_test_class_win_include_all_stuff &&);
    ~gr_test_class_win_include_all_stuff();

signals:

private:
    QSharedDataPointer<gr_test_class_win_include_all_stuffData> data;
};
