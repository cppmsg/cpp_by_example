#include <condition_variable>
#include <iostream>
#include <queue>
#include <memory>
#include <mutex>
#include "listing_c.classes.cpp"

int main() {
    Bank_cloud_smachine  bank_cloud_smachine{};
    Interface_smachine   interface_hardware_smachine{};
    Atm_smachine         atm_smachine      {bank_cloud_smachine.get_sender(), interface_hardware_smachine.get_sender()};
    std::thread         bank_thread      {&Bank_cloud_smachine::run, &bank_cloud_smachine};
    std::thread         interface_thread {&Interface_smachine::run, &interface_hardware_smachine };
    std::thread         atm_thread       {&Atm_smachine::run,&atm_smachine};
    Messaging::Sender   atm_sender       {atm_smachine.get_sender()};
    bool                quit_pressed     {false};
    while(!quit_pressed) {
        char c=getchar();
        switch(c) {
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            atm_sender.send(Msg::Digit_pressed(c));
            break;
        case 'b':
            atm_sender.send(Msg::Balance_pressed());
            break;
        case 'w':
            atm_sender.send(Msg::Withdraw_pressed(50));
            break;
        case 'c':
            atm_sender.send(Msg::Cancel_pressed());
            break;
        case 'q':
            quit_pressed=true;
            break;
        case 'i':
            atm_sender.send(Msg::Card_inserted("acc1234"));
            break;
        }
    }
    bank_cloud_smachine.done(); atm_smachine.done(); interface_hardware_smachine.done();
    atm_thread.join(); bank_thread.join(); interface_thread.join();
    std::cout << "###" << std::endl;
}
