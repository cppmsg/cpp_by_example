#include <condition_variable>
#include <iostream>
#include <queue>
#include <memory>
#include <mutex>
namespace Messaging {
    struct Message_base {
        virtual ~Message_base() {}
    };

    template<typename Msg>
    struct Wrapped_message: Message_base {
        Msg contents;
        explicit Wrapped_message(Msg const& contents_): contents(contents_) {}
    };

    class Queue {
        std::mutex                                  m;
        std::condition_variable                     c;
        std::queue< std::shared_ptr<Message_base> > q;
    public:
        template<typename T>
        void push(T const& msg) {
            std::lock_guard<std::mutex> lk(m);
            q.push(std::make_shared<Wrapped_message<T> >(msg));
            c.notify_all();
        }
        std::shared_ptr<Message_base> wait_and_pop() {
            std::unique_lock<std::mutex> lk(m);
            c.wait(lk,[&]{return !q.empty();});
            auto res=q.front();
            q.pop();
            return res;
        }
    };

    template<typename PreviousDispatcher,typename Msg,typename Func>
    class TemplateDispatcher {
        Queue *              q{};
        PreviousDispatcher * prev{};
        Func                 f{};
        bool                 chained{false};

        TemplateDispatcher(TemplateDispatcher const&)=delete;
        TemplateDispatcher& operator=(TemplateDispatcher const&)=delete;
        template<typename Dispatcher,typename OtherMsg,typename OtherFunc>
        friend class TemplateDispatcher;
        void wait_and_dispatch() {
            for(;;) {
                auto msg=q->wait_and_pop();
                if(dispatch(msg))
                    break;
            }
        }

        bool dispatch(std::shared_ptr<Message_base> const& msg) {
            if(Wrapped_message<Msg>* wrapper= dynamic_cast<Wrapped_message<Msg>*>(msg.get())) {
                f(wrapper->contents);
                return true;
            }
            else { return prev->dispatch(msg); }
        }
    public:
        TemplateDispatcher(TemplateDispatcher&& other_)
            : q(other_.q),prev(other_.prev),f(std::move(other_.f)), chained(other_.chained)
        {
            other_.chained=true;
        }
        TemplateDispatcher(Queue * q_, PreviousDispatcher * prev_, Func && f_)
            : q(q_),prev(prev_), f(std::forward<Func>(f_)), chained(false)
        {
            prev_->chained=true;
        }

        template<typename OtherMsg,typename OtherFunc>
        TemplateDispatcher<TemplateDispatcher,OtherMsg,OtherFunc>
        handle(OtherFunc&& of) {
            return TemplateDispatcher<
                TemplateDispatcher
                ,OtherMsg
                ,OtherFunc>( q, this, std::forward<OtherFunc>(of) );
        }
        ~TemplateDispatcher() noexcept(false) { if(!chained) { wait_and_dispatch(); } }
    };

    class Close_queue {};
    class Dispatcher {
        Queue *     q;
        bool        chained{false};  // grostig added false
        Dispatcher(Dispatcher const&)           =delete;
        Dispatcher& operator=(Dispatcher const&)=delete;

        template<
            typename Dispatcher,
            typename Msg,
            typename Func>
        friend class TemplateDispatcher;

        void wait_and_dispatch() {
            for(;;) {
                auto msg=q->wait_and_pop();
                dispatch(msg);
            }
        }
        bool dispatch( std::shared_ptr<Message_base> const& msg) {
            if(dynamic_cast<Wrapped_message<Close_queue>*>(msg.get())) { throw Close_queue(); }
            return false;
        }
    public:
        Dispatcher(Dispatcher&& other)
            : q(other.q),chained(other.chained) {
            other.chained=true;
        }
        explicit Dispatcher(Queue* q_): q(q_),chained(false) {}

        template<typename Message,typename Func>
        TemplateDispatcher<Dispatcher,Message,Func> handle(Func&& f) {
            return TemplateDispatcher<Dispatcher,Message,Func> {
                                                                  q, this ,std::forward<Func>(f)};
        }

        ~Dispatcher() noexcept(false) { if(!chained) { wait_and_dispatch(); } }
    };

    class Sender {
        Queue * q;
    public:
                 Sender()          : q(nullptr) {}
        explicit Sender(Queue * q_): q(q_) {}

        template<typename Message>
        void send(Message const& msg) {
            if(qdisplay_insufficient_funds) { q->push(msg); }
        }
    };
    class Receiver {
        Queue q;
    public:
        operator Sender() { return Sender(&q); }
        Dispatcher wait() { return Dispatcher(&q); }
    };
} // END namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

struct Withdraw {
    std::string account;
    unsigned amount;
    mutable Messaging::Sender atm_queue;
    Withdraw(std::string const& account_, unsigned amount_, Messaging::Sender atm_queue_)
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
class Atm {
    Messaging::Receiver incoming{};
    Messaging::Sender   bank{};
    Messaging::Sender   interface_hardware{};
    std::string         account{};
    unsigned            withdrawal_amount{};
    std::string         pin{};
    void (Atm::*state)();  // TODO??: what is this?
    void process_withdrawal() {
        incoming.wait()
            .handle<Withdraw_ok>( [&](Withdraw_ok const& msg) {
                    interface_hardware.send(
                        Issue_money(withdrawal_amount));
                    bank.send(
                        Withdrawal_processed(account,withdrawal_amount));
                    state=&Atm::done_processing;
                } )
            .handle<withdraw_denied>(
                [&](withdraw_denied const& msg) {
                    interface_hardware.send(Display_insufficient_funds());
                    state=&Atm::done_processing;
                } )
            .handle<Cancel_pressed>(
                [&](Cancel_pressed const& msg) {
                    bank.send(
                        Cancel_withdrawal(account,withdrawal_amount));
                    interface_hardware.send(
                        Display_withdrawal_cancelled());
                    state=&Atm::done_processing;
                } );
    }
    void process_balance() {
        incoming.wait()
            .handle<Balance>(
                [&](Balance const& msg) {
                    interface_hardware.send(Display_balance(msg.amount));
                    state=&Atm::wait_for_action;
                } )
            .handle<Cancel_pressed>(
                [&](Cancel_pressed const& msg) {
                    state=&Atm::done_processing;
                } );
    }
    void wait_for_action() {
        interface_hardware.send(Display_withdrawal_options());
        incoming.wait() .handle<Withdraw_pressed>(
                [&](Withdraw_pressed const& msg) {
                    withdrawal_amount=msg.amount;
                    bank.send( Withdraw( account, msg.amount, incoming) );
                    state=&Atm::process_withdrawal;
                } )
            .handle<Balance_pressed>(
                [&](Balance_pressed const& msg) {
                    bank.send(Get_balance(account,incoming));
                    state=&Atm::process_balance;
                } )
            .handle<Cancel_pressed>(
                [&](Cancel_pressed const& msg) {
                    state=&Atm::done_processing;
                } );
    }
    void verifying_pin() {
        incoming.wait()
            .handle<Pin_verified>(
                [&](Pin_verified const& msg) {
                    state=&Atm::wait_for_action;
                } )
            .handle<Pin_incorrect>(
                [&](Pin_incorrect const& msg)
                {   interface_hardware.send( Display_pin_incorrect_message());
                    state=&Atm::done_processing;
                } )
            .handle<Cancel_pressed>(
                [&](Cancel_pressed const& msg) {
                    state=&Atm::done_processing;
                } );
    }
    void getting_pin() {
        incoming.wait()
            .handle<Digit_pressed>(
                [&](Digit_pressed const& msg) {
                    unsigned const pin_length=4;
                    pin+=msg.digit;
                    if(pin.length()==pin_length) {
                        bank.send(Verify_pin(account,pin,incoming));
                        state=&Atm::verifying_pin;
                    }
                } )
            .handle<Clear_last_pressed>(
                [&](Clear_last_pressed const& msg) {
                    if(!pin.empty()) { pin.pop_back(); }
                } )
            .handle<Cancel_pressed>(
                [&](Cancel_pressed const& msg) {
                    state=&Atm::done_processing;
                } );
    }
    void waiting_for_card() {
        interface_hardware.send(Display_enter_card());
        incoming.wait() .handle<Card_inserted>(
                [&](Card_inserted const& msg) {
                    account=msg.account;
                    pin="";
                    interface_hardware.send(Display_enter_pin());
                    state=&Atm::getting_pin;
                } );
    }
    void done_processing() {
        interface_hardware.send(Eject_card());
        state=&Atm::waiting_for_card;
    }
    Atm(Atm const&)=delete;
    Atm& operator=(Atm const&)=delete;
public:
    Atm(Messaging::Sender bank_, Messaging::Sender interface_hardware_) : bank(bank_),interface_hardware(interface_hardware_) {}
    void done() { get_sender().send(Messaging::Close_queue()); }
    void run() {
        state=&Atm::waiting_for_card;
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
class Bank_machine {
    Messaging::Receiver incoming;
    unsigned balance;
public:
    Bank_machine(): balance(199) {}
    void done() { get_sender().send(Messaging::Close_queue()); }
    void run() { try { for(;;) {
                incoming.wait() .handle<Verify_pin>(
                        [&](Verify_pin const& msg) {
                            if(msg.pin=="mypin") { msg.atm_queue.send(Pin_verified()); }
                            else { msg.atm_queue.send(Pin_incorrect()); }
                        } )
                    .handle<Withdraw>(
                        [&](Withdraw const& msg) {
                            if(balance>=msg.amount) {
                                msg.atm_queue.send(Withdraw_ok());
                                balance-=msg.amount;
                            }
                            else {
                                msg.atm_queue.send(withdraw_denied());
                            }
                        } )
                    .handle<Get_balance>(
                        [&](Get_balance const& msg) {
                            msg.atm_queue.send(::balance(balance));
                        } )
                    .handle<Withdrawal_processed>(
                        [&](Withdrawal_processed const& msg) { }
                        )
                    .handle<Cancel_withdrawal>(
                        [&](Cancel_withdrawal const& msg) { }
                        );
            } } catch(Messaging::Close_queue const&) {}
    }
    Messaging::Sender get_sender() { return incoming; }
};

class Interface_machine {
    std::mutex          iom;
    Messaging::Receiver incoming;
public:
    void done() { get_sender().send(Messaging::Close_queue()); }
    void run() { try {
            for(;;)
            {
                incoming.wait()
                    .handle<Issue_money>(
                        [&](Issue_money const& msg) {
                            {
                                std::lock_guard<std::mutex> lk{iom};
                                 std::cout<<"Issuing "
                                         <<msg.amount<<std::endl;
                            }
                        } )
                    .handle<Display_insufficient_funds>(
                        [&](Display_insufficient_funds const& msg) {
                            {
                                std::lock_guard<std::mutex> lk{iom};
                                std::cout<<"Insufficient funds"<<std::endl;
                            }
                        } )
                    .handle<Display_enter_pin>(
                        [&](Display_enter_pin const& msg) {
                            {
                                std::lock_guard<std::mutex> lk{iom};
                                std::cout
                                    <<"Please enter your PIN (0-9)"
                                    <<std::endl;
                            }
                        } )
                    .handle<Display_enter_card>(
                        [&](Display_enter_card const& msg) { {
                                std::lock_guard<std::mutex> lk{iom};
                                std::cout<<"Please enter your card (I)"
                                         <<std::endl;
                            }
                        } )
                    .handle<Display_balance>(
                        [&](Display_balance const& msg) { {
                                std::lock_guard<std::mutex> lk{iom};
                                 std::cout
                                    <<"The balance of your account is "
                                    <<msg.amount<<std::endl;
                            }
                        } )
                    .handle<Display_withdrawal_options>(
                        [&](Display_withdrawal_options const& msg) { {
                                std::lock_guard<std::mutex> lk{iom};
                                std::cout<<"Withdraw 50? (w)"<<std::endl;
                                std::cout<<"Display Balance? (b)"
                                         <<std::endl;
                                std::cout<<"Cancel? (c)"<<std::endl;
                            }
                        }
                        )
                    .handle<Display_withdrawal_cancelled>(
                        [&](Display_withdrawal_cancelled const& msg) { {
                                std::lock_guard<std::mutex> lk{iom};
                                std::cout<<"Withdrawal cancelled"
                                         <<std::endl;
                            }
                        } )
                    .handle<Display_pin_incorrect_message>(
                        [&](Display_pin_incorrect_message const& msg) { {
                                std::lock_guard<std::mutex> lk{iom};
                                std::cout<<"PIN incorrect"<<std::endl;
                            }
                        } )
                    .handle<Eject_card>(
                        [&](Eject_card const& msg) { {
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

int main() {
    Bank_machine        bank;
    Interface_machine   interface_hardware;
    Atm                 machine(bank.get_sender(),interface_hardware.get_sender());
    std::thread         bank_thread(&Bank_machine::run,&bank);
    std::thread         if_thread(&Interface_machine::run,&interface_hardware);
    std::thread         atm_thread(&Atm::run,&machine);
    Messaging::Sender   atmqueue(machine.get_sender());
    bool                quit_pressed=false;
    while(!quit_pressed) {
        char c=getchar();
        switch(c) {
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            atmqueue.send(Digit_pressed(c));
            break;
        case 'b':
            atmqueue.send(Balance_pressed());
            break;
        case 'w':
            atmqueue.send(Withdraw_pressed(50));
            break;
        case 'c':
            atmqueue.send(Cancel_pressed());
            break;
        case 'q':
            quit_pressed=true;
            break;
        case 'i':
            atmqueue.send(Card_inserted("acc1234"));
            break;
        }
    }
    bank.done();
    machine.done();
    interface_hardware.done();
    atm_thread.join();
    bank_thread.join();
    if_thread.join();
}
