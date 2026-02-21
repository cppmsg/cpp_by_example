#include <condition_variable>
#include <iostream>
#include <queue>
#include <memory>
#include <mutex>
namespace Messaging {
struct Message_base {
    virtual ~Message_base() {}  // must have boilerplate.
};

template <typename Msg>
struct Wrapped_message : Message_base {  // TODO??: define in detail "wrapped" in this context and in general? It it just to use runtime dispatch?
    Msg contents{};
    explicit Wrapped_message(Msg const &contents_) : contents(contents_) {}
};
class Queue {
    std::mutex                                m{};
    std::condition_variable                   cond_var{};
    std::queue<std::shared_ptr<Message_base>> queue_sptr_to_wrapped_msg{};
public:
    template <typename T>
    void push(T const &msg) {
        std::lock_guard<std::mutex> lk(m);
        queue_sptr_to_wrapped_msg.push(std::make_shared<Wrapped_message<T>>(msg));
        cond_var.notify_all();
    }
    std::shared_ptr<Message_base> wait_and_pop() {
        std::unique_lock<std::mutex> lk(m);
        cond_var.wait(lk, [&] { return not queue_sptr_to_wrapped_msg.empty(); });
        auto result = queue_sptr_to_wrapped_msg.front();
        queue_sptr_to_wrapped_msg.pop();
        return result;
    }
};

template <typename PreviousDispatcher, typename Msg, typename Func> class TemplateDispatcher {
    Queue              *ptr_queue{};
    PreviousDispatcher *ptr_prev_dispatcher{};
    Func                f{};
    bool                is_chained{ false };

    TemplateDispatcher(TemplateDispatcher const &)            = delete;
    TemplateDispatcher &operator=(TemplateDispatcher const &) = delete;

    template <typename Dispatcher, typename OtherMsg, typename OtherFunc> friend class TemplateDispatcher;
    void wait_and_dispatch() {
        for(;;) {
            auto msg = ptr_queue->wait_and_pop();
            if(dispatch(msg)) break;
        }
    }
    bool dispatch(std::shared_ptr<Message_base> const &msg) {
        if(Wrapped_message<Msg> *wrapper = dynamic_cast<Wrapped_message<Msg> *>(msg.get())) {
            f(wrapper->contents);
            return true;
        } else {
            return ptr_prev_dispatcher->dispatch(msg);
        }
    }
public:
    TemplateDispatcher(TemplateDispatcher &&other_)
        : ptr_queue(other_.ptr_queue), ptr_prev_dispatcher(other_.ptr_prev_dispatcher), f(std::move(other_.f)), is_chained(other_.is_chained) {
        other_.is_chained = true;
    }
    TemplateDispatcher(Queue *q_, PreviousDispatcher *prev_, Func &&f_)
        : ptr_queue(q_), ptr_prev_dispatcher(prev_), f(std::forward<Func>(f_)), is_chained(false) {
        prev_->is_chained = true;
    }

    template <typename OtherMsg, typename OtherFunc>
    TemplateDispatcher<TemplateDispatcher, OtherMsg, OtherFunc> handle(OtherFunc &&of) {
        return TemplateDispatcher<TemplateDispatcher, OtherMsg, OtherFunc>(ptr_queue, this, std::forward<OtherFunc>(of));
    }
    ~TemplateDispatcher() noexcept(false) {
        if(!is_chained) { wait_and_dispatch(); }
    }
};

class Close_queue {};
class Dispatcher {
    Queue *q;
    bool   is_chained{ false }; // grostig added false
    Dispatcher(Dispatcher const &)            = delete;
    Dispatcher &operator=(Dispatcher const &) = delete;

    template <typename Dispatcher, typename Msg, typename Func>

    friend class TemplateDispatcher;

    void
    wait_and_dispatch() {
        for(;;) {
            auto msg = q->wait_and_pop();
            dispatch(msg);
        }
    }
    bool
    dispatch(std::shared_ptr<Message_base> const &msg) {
        if(dynamic_cast<Wrapped_message<Close_queue> *>(msg.get())) { throw Close_queue(); }
        return false;
    }
public:
    Dispatcher(Dispatcher &&other) : q(other.q), is_chained(other.is_chained) { other.is_chained = true; }
    explicit Dispatcher(Queue *q_) : q(q_), is_chained(false) {}

    template <typename Message, typename Func>
    TemplateDispatcher<Dispatcher, Message, Func>
    handle(Func &&f) {
        return TemplateDispatcher<Dispatcher, Message, Func>{ q, this, std::forward<Func>(f) };
    }
    ~Dispatcher() noexcept(false) {
        if(!is_chained) { wait_and_dispatch(); }
    }
};

class Sender {
    Queue *q;
public:
    Sender() : q(nullptr) {}
    explicit Sender(Queue *q_) : q(q_) {}

    template <typename Message>
    void send(Message const &msg) {
        if(q) { q->push(msg); }
    }
};
class Receiver {
    Queue q;
public:
    operator Sender() { return Sender(&q); }
    Dispatcher wait() {
        return Dispatcher(&q);
    }
};
} // END namespace Messaging NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

namespace Msg {
struct Withdraw {
    std::string account;
    unsigned amount;
    mutable Messaging::Sender atm_queue;
    Withdraw(std::string const& account_, unsigned amount_, Messaging::Sender atm_queue_)
        : account(account_),amount(amount_), atm_queue(atm_queue_) {}
};
struct Withdraw_ok {};
struct Withdraw_denied {};
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
    mutable Messaging::Sender atm_queue;
    Verify_pin(std::string const& account_,std::string const& pin_, Messaging::Sender atm_queue_)
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
    std::string account{};
    mutable Messaging::Sender atm_queue{};
    Get_balance(std::string const& account_,Messaging::Sender atm_queue_)
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
} // END namespace Msg NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

class Atm_smachine {
    Messaging::Receiver incoming{};
    Messaging::Sender   bank{};
    Messaging::Sender   interface_hardware{};
    std::string         account{};
    std::string         pin{};
    unsigned            withdrawal_amount{};

    void (Atm_smachine::*state)();  // TODO??: what is this?
    void process_withdrawal() {
        incoming.wait()
            .handle<Msg::Withdraw_ok>( [&](Msg::Withdraw_ok const& msg) {
                    interface_hardware.send(
                        Msg::Issue_money(withdrawal_amount));
                    bank.send(
                        Msg::Withdrawal_processed(account,withdrawal_amount));
                    state=&Atm_smachine::done_processing;
                } )
            .handle<Msg::Withdraw_denied>(
                [&](Msg::Withdraw_denied const& msg) {
                    interface_hardware.send(Msg::Display_insufficient_funds());
                    state=&Atm_smachine::done_processing;
                } )
            .handle<Msg::Cancel_pressed>(
                [&](Msg::Cancel_pressed const& msg) {
                    bank.send(
                        Msg::Cancel_withdrawal(account,withdrawal_amount));
                    interface_hardware.send(
                        Msg::Display_withdrawal_cancelled());
                    state=&Atm_smachine::done_processing;
                } );
    }
    void process_balance() {
        incoming.wait()
            .handle<Msg::Balance>(
                [&](Msg::Balance const& msg) {
                    interface_hardware.send(Msg::Display_balance(msg.amount));
                    state=&Atm_smachine::wait_for_action;
                } )
            .handle<Msg::Cancel_pressed>(
                [&](Msg::Cancel_pressed const& msg) {
                    state=&Atm_smachine::done_processing;
                } );
    }
    void wait_for_action() {
        interface_hardware.send(Msg::Display_withdrawal_options());
        incoming.wait() .handle<Msg::Withdraw_pressed>(
                [&](Msg::Withdraw_pressed const& msg) {
                    withdrawal_amount=msg.amount;
                    bank.send( Msg::Withdraw( account, msg.amount, incoming) );
                    state=&Atm_smachine::process_withdrawal;
                } )
            .handle<Msg::Balance_pressed>(
                [&](Msg::Balance_pressed const& msg) {
                    bank.send(Msg::Get_balance(account,incoming));
                    state=&Atm_smachine::process_balance;
                } )
            .handle<Msg::Cancel_pressed>(
                [&](Msg::Cancel_pressed const& msg) {
                    state=&Atm_smachine::done_processing;
                } );
    }
    void verifying_pin() {
        incoming.wait()
            .handle<Msg::Pin_verified>(
                [&](Msg::Pin_verified const& msg) {
                    state=&Atm_smachine::wait_for_action;
                } )
            .handle<Msg::Pin_incorrect>(
                [&](Msg::Pin_incorrect const& msg)
                {   interface_hardware.send( Msg::Display_pin_incorrect_message());
                    state=&Atm_smachine::done_processing;
                } )
            .handle<Msg::Cancel_pressed>(
                [&](Msg::Cancel_pressed const& msg) {
                    state=&Atm_smachine::done_processing;
                } );
    }
    void getting_pin() {
        incoming.wait()
            .handle<Msg::Digit_pressed>(
                [&](Msg::Digit_pressed const& msg) {
                    unsigned const pin_length=4;
                    pin+=msg.digit;
                    if(pin.length()==pin_length) {
                        bank.send(Msg::Verify_pin(account,pin,incoming));
                        state=&Atm_smachine::verifying_pin;
                    }
                } )
            .handle<Msg::Clear_last_pressed>(
                [&](Msg::Clear_last_pressed const& msg) {
                    if(!pin.empty()) { pin.pop_back(); }
                } )
            .handle<Msg::Cancel_pressed>(
                [&](Msg::Cancel_pressed const& msg) {
                    state=&Atm_smachine::done_processing;
                } );
    }
    void waiting_for_card() {
        interface_hardware.send(Msg::Display_enter_card());
        incoming.wait() .handle<Msg::Card_inserted>(
                [&](Msg::Card_inserted const& msg) {
                    account=msg.account;
                    pin="";
                    interface_hardware.send(Msg::Display_enter_pin());
                    state=&Atm_smachine::getting_pin;
                } );
    }
    void done_processing() {
        interface_hardware.send(Msg::Eject_card());
        state=&Atm_smachine::waiting_for_card;
    }
    Atm_smachine(Atm_smachine const&)=delete;
    Atm_smachine& operator=(Atm_smachine const&)=delete;
public:
    Atm_smachine(Messaging::Sender bank_, Messaging::Sender interface_hardware_) : bank(bank_),interface_hardware(interface_hardware_) {}
    void done() { get_sender().send(Messaging::Close_queue()); }
    void run() {
        state=&Atm_smachine::waiting_for_card;
        try {
            for(;;)
            {
                (this->*state)();
            }
        }
        catch(Messaging::Close_queue const&) { }
    }
    Messaging::Sender get_sender() {
        return incoming;
    }
};

class Bank_cloud_smachine {
    unsigned balance{};
    Messaging::Receiver incoming{};
public:
    Bank_cloud_smachine(): balance{199} {}
    void done() { get_sender().send(Messaging::Close_queue()); }
    void run() { try { for(;;) {
                incoming.wait()
                    .handle<Msg::Verify_pin>(
                        [&](Msg::Verify_pin const& msg) {
                            if(msg.pin=="mypin") { msg.atm_queue.send(Msg::Pin_verified()); }
                            else { msg.atm_queue.send(Msg::Pin_incorrect()); }
                        } )
                    .handle<Msg::Withdraw>(
                        [&](Msg::Withdraw const& msg) {
                            if(balance>=msg.amount) {
                                msg.atm_queue.send(Msg::Withdraw_ok());
                                balance-=msg.amount;
                            }
                            else { msg.atm_queue.send(Msg::Withdraw_denied()); }
                        } )
                    .handle<Msg::Get_balance>(
                        [&](Msg::Get_balance const& msg) {
                            msg.atm_queue.send(Msg::Balance(balance));
                        } )
                    .handle<Msg::Withdrawal_processed>(
                        [&](Msg::Withdrawal_processed const& msg) {
                        } )
                    .handle<Msg::Cancel_withdrawal>(
                        [&](Msg::Cancel_withdrawal const& msg) {
                        } );
            } } catch(Messaging::Close_queue const&) {}
    }
    Messaging::Sender get_sender() { return incoming; }
};

class Interface_smachine {
    std::mutex          iom;
    Messaging::Receiver incoming;
public:
    void done() { get_sender().send(Messaging::Close_queue()); }
    void run() { try { for(;;) {
                incoming.wait()
                    .handle<Msg::Issue_money>(
                        [&](Msg::Issue_money const& msg) { {
                                std::lock_guard<std::mutex> lk{iom};
                                std::cout<<"Issuing "<<msg.amount<<std::endl;
                            }
                        } )
                    .handle<Msg::Display_insufficient_funds>(
                        [&](Msg::Display_insufficient_funds const& msg) { {
                                std::lock_guard<std::mutex> lk{iom};
                                std::cout<<"Insufficient funds"<<std::endl;
                            }
                        } )
                    .handle<Msg::Display_enter_pin>(
                        [&](Msg::Display_enter_pin const& msg) { {
                                std::lock_guard<std::mutex> lk{iom};
                                std::cout <<"Please enter your PIN (0-9 hint: mypin)" <<std::endl;
                            }
                        } )
                    .handle<Msg::Display_enter_card>(
                        [&](Msg::Display_enter_card const& msg) { {
                                std::lock_guard<std::mutex> lk{iom};
                                std::cout<<"Please enter your card (I)" <<std::endl;
                            }
                        } )
                    .handle<Msg::Display_balance>(
                        [&](Msg::Display_balance const& msg) { {
                                std::lock_guard<std::mutex> lk{iom};
                                std::cout <<"The balance of your account is " <<msg.amount<<std::endl;
                            }
                        } )
                    .handle<Msg::Display_withdrawal_options>(
                        [&](Msg::Display_withdrawal_options const& msg) { {
                                std::lock_guard<std::mutex> lk{iom};
                                std::cout<<"Withdraw 50? (w)"<<std::endl;
                                std::cout<<"Display Balance? (b)"
                                         <<std::endl;
                                std::cout<<"Cancel? (c)"<<std::endl;
                            }
                        }
                        )
                    .handle<Msg::Display_withdrawal_cancelled>(
                        [&](Msg::Display_withdrawal_cancelled const& msg) { {
                                std::lock_guard<std::mutex> lk{iom};
                                std::cout<<"Withdrawal cancelled"
                                         <<std::endl;
                            }
                        } )
                    .handle<Msg::Display_pin_incorrect_message>(
                        [&](Msg::Display_pin_incorrect_message const& msg) { {
                                std::lock_guard<std::mutex> lk{iom};
                                std::cout<<"PIN incorrect"<<std::endl;
                            }
                        } )
                    .handle<Msg::Eject_card>(
                        [&](Msg::Eject_card const& msg) { {
                                std::lock_guard<std::mutex> lk{iom};
                                std::cout<<"Ejecting card"<<std::endl;
                            }
                        } );
            }
        }
        catch(Messaging::Close_queue&) {};
    }
    Messaging::Sender get_sender() { return incoming; }
};
