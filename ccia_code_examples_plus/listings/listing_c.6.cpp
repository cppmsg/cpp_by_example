#include <string>
struct withdraw {
    std::string account;
    unsigned amount;
    mutable Messaging::sender atm_queue;
    withdraw(std::string const& account_, unsigned amount_, Messaging::sender atm_queue_)
        : account(account_),amount(amount_), atm_queue(atm_queue_) {}
};
struct Withdraw_ok {};
struct withdraw_denied {};
struct Cancel_withdrawal {
    std::string account;
    unsigned amount;
    Cancel_withdrawal(std::string const& account_, unsigned amount_)
        : account(account_),amount(amount_) {}
};
struct Withdrawal_processed {
    std::string account;
    unsigned amount;
    Withdrawal_processed(std::string const& account_, unsigned amount_): account(account_),amount(amount_) {}
};
struct Card_inserted {
    std::string account;
    explicit Card_inserted(std::string const& account_): account(account_) {}
};
struct Digit_pressed {
    char digit;
    explicit Digit_pressed(char digit_): digit(digit_) {}
};
struct Clear_last_pressed {};
struct Eject_card {};
struct Withdraw_pressed {
    unsigned amount;
    explicit Withdraw_pressed(unsigned amount_): amount(amount_) {}
};
struct Cancel_pressed {};
struct Issue_money {
    unsigned amount;
    Issue_money(unsigned amount_): amount(amount_) {}
};
struct Verify_pin {
    std::string account;
    std::string pin;
    mutable Messaging::sender atm_queue;
    Verify_pin(std::string const& account_,std::string const& pin_, Messaging::sender atm_queue_)
        : account(account_),pin(pin_),atm_queue(atm_queue_) {}
};
struct Pin_verified {};
struct Pin_incorrect {};
struct Display_enter_pin {};
struct Display_enter_card {};
struct Display_insufficient_funds {};
struct Display_withdrawal_cancelled {};
struct Display_pin_incorrect_message {};
struct Display_withdrawal_options {};
struct Get_balance {
    std::string account;
    mutable Messaging::sender atm_queue;
    Get_balance(std::string const& account_,Messaging::sender atm_queue_)
        : account(account_),atm_queue(atm_queue_) {}
};
struct Balance {
    unsigned amount;
    explicit Balance(unsigned amount_): amount(amount_) {}
};
struct Display_balance {
    unsigned amount;
    explicit Display_balance(unsigned amount_): amount(amount_) {}
};
struct Balance_pressed {};
