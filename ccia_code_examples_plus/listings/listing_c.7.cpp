#include <string>
class Atm {
    Messaging::receiver incoming;
    Messaging::sender bank;
    Messaging::sender interface_hardware;
    void (Atm::*state)();
    std::string account;
    unsigned withdrawal_amount;
    std::string pin;
    void process_withdrawal() {
        incoming.wait()
            .handle<withdraw_ok>( [&](withdraw_ok const& msg) {
                    interface_hardware.send(
                        issue_money(withdrawal_amount));
                    bank.send(
                        withdrawal_processed(account,withdrawal_amount));
                    state=&Atm::done_processing;
                } )
            .handle<withdraw_denied>(
                [&](withdraw_denied const& msg) {
                    interface_hardware.send(display_insufficient_funds());
                    state=&Atm::done_processing;
                } )
            .handle<cancel_pressed>(
                [&](cancel_pressed const& msg) {
                    bank.send(
                        cancel_withdrawal(account,withdrawal_amount));
                    interface_hardware.send(
                        display_withdrawal_cancelled());
                    state=&Atm::done_processing;
                } );
    }
    void process_balance() {
        incoming.wait()
            .handle<balance>(
                [&](balance const& msg) {
                    interface_hardware.send(display_balance(msg.amount));
                    state=&Atm::wait_for_action;
                } )
            .handle<cancel_pressed>(
                [&](cancel_pressed const& msg) {
                    state=&Atm::done_processing;
                } );
    }
    void wait_for_action() {
        interface_hardware.send(display_withdrawal_options());
        incoming.wait() .handle<withdraw_pressed>(
                [&](withdraw_pressed const& msg) {
                    withdrawal_amount=msg.amount;
                    bank.send(withdraw(account,msg.amount,incoming));
                    state=&Atm::process_withdrawal;
                } )
            .handle<balance_pressed>(
                [&](balance_pressed const& msg) {
                    bank.send(get_balance(account,incoming));
                    state=&Atm::process_balance;
                } )
            .handle<cancel_pressed>(
                [&](cancel_pressed const& msg) {
                    state=&Atm::done_processing;
                } );
    }
    void verifying_pin() {
        incoming.wait()
            .handle<pin_verified>(
                [&](pin_verified const& msg) {
                    state=&Atm::wait_for_action;
                } )
            .handle<pin_incorrect>(
                [&](pin_incorrect const& msg)
                {   interface_hardware.send( display_pin_incorrect_message());
                    state=&Atm::done_processing;
                } )
            .handle<cancel_pressed>(
                [&](cancel_pressed const& msg) {
                    state=&Atm::done_processing;
                } );
    }
    void getting_pin() {
        incoming.wait()
            .handle<digit_pressed>(
                [&](digit_pressed const& msg) {
                    unsigned const pin_length=4;
                    pin+=msg.digit;
                    if(pin.length()==pin_length) {
                        bank.send(verify_pin(account,pin,incoming));
                        state=&Atm::verifying_pin;
                    }
                } )
            .handle<clear_last_pressed>(
                [&](clear_last_pressed const& msg) {
                    if(!pin.empty()) { pin.pop_back(); }
                } )
            .handle<cancel_pressed>(
                [&](cancel_pressed const& msg) {
                    state=&Atm::done_processing;
                } );
    }
    void waiting_for_card() {
        interface_hardware.send(display_enter_card());
        incoming.wait() .handle<card_inserted>(
                [&](card_inserted const& msg) {
                    account=msg.account;
                    pin="";
                    interface_hardware.send(display_enter_pin());
                    state=&Atm::getting_pin;
                } );
    }
    void done_processing() {
        interface_hardware.send(eject_card());
        state=&Atm::waiting_for_card;
    }
    Atm(Atm const&)=delete;
    Atm& operator=(Atm const&)=delete;
public:
    Atm(Messaging::sender bank_, Messaging::sender interface_hardware_) : bank(bank_),interface_hardware(interface_hardware_) {}
    void done() { get_sender().send(Messaging::close_queue()); }
    void run() {
        state=&Atm::waiting_for_card;
        try {
            for(;;)
            {
                (this->*state)();
            }
        }
        catch(Messaging::close_queue const&) { }
    }
    Messaging::sender get_sender() {
        return incoming;
    }
};
