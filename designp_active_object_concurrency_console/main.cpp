/** Copyright (c) Grant Rostig, grantrostig.com 2023. Distributed under the Boost Software License, Version 1.0. (See accompanying
   file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt) NOT PRODUCTION QUALITY CODE, just shows learning/teaching
   example, not real programming, don't copy this style, just playing around Reminder of usefull resources:
        https://coliru.stacked-crooked.com/
        https://godbolt.org/
        https://cppinsights.io/
        https://Wandbox.org
        https://cpp.sh/
        https://quick-bench.com/
        https://arnemertz.github.io/online-compilers/
    [[maybe_unused]] int * my_new_var8 = ::new (22,int);
    Language Versions:  clan++ -std=c++2b, g++ -std=c++23 or =gnu++23
    STD Libraries: Gcc: libstdc++; Clang: libc++; Microsoft: msvc/ms stl  // TODO??: how do we link with different libraries than the
   default using gcc or clang (the exact command line text)?
    g++ -Werror -Weffc++ -Wextra -Wall -Wconversion -Wshadow -Wpedantic -Wold-style-cast -Wsign-promo -Wzero-as-null-pointer-constant
   -Wsuggest-override -Wnon-virtual-dtor \
        -Wcast-align -Woverloaded-virtual -Wunused -pedantic -Wsign-conversion -Wmisleading-indentation -Wnull-dereference
   -Wdouble-promotion -Wformat=2 -Wimplicit-fallthrough \
        -Wuseless-cast -Wsuggest-final-types -Wsuggest-final-methods -Wduplicated-cond -Wduplicated-branches -Wlogical-op
   -std=gnuc++23 \ main.cpp <OTHER>.cpp -o <A.OUT>

    SYMBOL    MEANING // for debugging purposes
    TODO:     the principal programmer needs todo.
    TODO?:	  the principal programmer is not sure about something, that should be addressed.
    TODO?:X   the X programmer is not sure about something, that should be addressed.
    TODO??:   is a question for verbal discussion at CppMSG.com meetup meetings.

    Ordering of headers as follows: re:Lakos
    + The prototype/interface header for this implementation (ie, the .h/.hh file that corresponds to this .cpp/.cc file).
    + Other headers from the same project, as needed.
    + Headers from other non-standard, non-system libraries (for example, Qt, Eigen, etc).
    + Headers from other "almost-standard" libraries (for example, Boost)
    + Standard C++ headers (for example, iostream, functional, etc.)
    + Standard C headers (for example, cstdint, dirent.h, etc.)

    define NDEBUG if asserts are NOT to be checked.  Put in *.h file not *.CPP
    #define NDEBUG
    define GR_DEBUG if we/programmer is Debugging.  Put in *.h file not *.CPP
    #define GR_DEBUG
    #ifdef GR_DEBUG
    #endif GR_DEBUG
 */
// other: derived from Grok LLM
#include "global_entities.hpp"
// #include "boost_headers.hpp"
// #include "cpp_headers.hpp"
// #include "math_grostig.hpp"
// #include "ostream_joiner_gr.hpp"
// #include "random_toolkit.hpp"
// #include <bits/stdc++.h>
// #include <boost/dynamic_bitset.hpp>
// #include <boost/multiprecision/cpp_int.hpp>
// #include <dlib/numeric_constants.h>
// #include <gsl/gsl>      // sudo dnf install  guidelines-support-library-devel
#include <atomic>
#include <bit>
#include <bitset>
#include <cassert>
#include <chrono>
#include <climits>
#include <cmath>
#include <condition_variable>
#include <csignal>
#include <flat_map>
#include <functional>
#include <future>
#include <iostream>
#include <memory>
#include <map>
#include <mutex>
#include <optional>
#include <queue>
#include <source_location>
#include <stacktrace>
#include <string>
#include <thread>
#include <vector>
using std::cerr;
using std::cin;
using std::clog;
using std::cout;
using std::endl; // NOT using namespace std; // duplicated here in case global_entities.hpp is not used.
using namespace std::string_literals;
using namespace std::chrono_literals;

namespace Detail { // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

void
f() {
    LOGGER_()
    LOGGER_()
}

} // END namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

namespace Example_sonnet { // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN derived from Sonnet LLM
class DataProcessor; // Forward declarations
class LogManager;
class MethodRequest { // METHOD REQUEST PATTERN - Base class for all method requests
public:
    virtual ~MethodRequest() = default;
    virtual void execute() = 0;
    virtual bool canExecute() const { return true; }
};

class ActivationQueue { // ACTIVATION QUEUE - Thread-safe queue for method requests
    std::queue<std::unique_ptr<MethodRequest>>  queue_;
    mutable std::mutex                          mutex_;
    std::condition_variable                     condition_;
    std::atomic<bool>                           shutdown_{false};
public:
    void enqueue(std::unique_ptr<MethodRequest> request) {
        std::lock_guard<std::mutex> lock(mutex_);
        if (!shutdown_) {
            queue_.push(std::move(request));
            condition_.notify_one();
        }
    }
    std::unique_ptr<MethodRequest>
    dequeue() {
        std::unique_lock<std::mutex> lock(mutex_);
        while (queue_.empty() && !shutdown_) {
            condition_.wait(lock);
        }
        if (shutdown_ && queue_.empty()) {
            return nullptr;
        }
        auto request = std::move(queue_.front());
        queue_.pop();
        return request;
    }
    void shutdown() {
        std::lock_guard<std::mutex> lock(mutex_);
        shutdown_ = true;
        condition_.notify_all();
    }
    bool empty() const {
        std::lock_guard<std::mutex> lock(mutex_);
        return queue_.empty();
    }
};

struct CustomerData {  // DATA STRUCTURES - Representing the 3 data fields
    std::string name{};
    std::string email{};
    int         age{};
    //CustomerData() : age(0) {}
    //CustomerData(const std::string & n, const std::string & e, int a) : name(n), email(e), age(a) {}
    //CustomerData(std::string n, std::string e, int a) : name(n), email(e), age(a) {}
    //CustomerData(std::string & n, std::string & e, int & a) : name(n), email(e), age(a) {}
};

struct ProductData {
    std::string productName{};
    std::string category{};
    double      price{};
    //ProductData() : price(0.0) {}
    //ProductData(const std::string & name, const std::string & cat, double p) : productName(name), category(cat), price(p) {}
};

struct OrderData {
    std::string orderId{};
    std::string customerId{};
    std::string productId{};
    int         quantity{};
    OrderData() : quantity(0) {}
    OrderData(const std::string& oid, const std::string& cid, const std::string& pid, int qty)
        : orderId(oid), customerId(cid), productId(pid), quantity(qty) {}
};

class DataProcessor { // ACTIVE OBJECT 1: DATA PROCESSOR
    ActivationQueue                     queue_;
    std::thread                         worker_;
    std::atomic<bool>                   running_{true};
    std::map<std::string, CustomerData> customers_; // Not Thread-safe data storage https://claude.ai/chat/085ea879-3093-404d-837a-68d7ae3248ba
    std::map<std::string, ProductData>  products_;
    std::map<std::string, OrderData>    orders_;
    //std::mutex                          data_mutex_;  // TODO??: why must a lock_guard use a non const std::mutex?
    mutable std::mutex                  data_mutex_;

    class ProcessCustomerRequest : public MethodRequest { // Method Request implementations
        DataProcessor* processor_;
        CustomerData data_;
        std::promise<bool> result_;
    public:
        ProcessCustomerRequest(DataProcessor* p, const CustomerData& data)
            : processor_(p), data_(data) {}
        void execute() override {
            bool success = processor_->processCustomerImpl(data_);
            result_.set_value(success);
        }
        std::future<bool> getFuture() { return result_.get_future(); }
    };

    class ProcessProductRequest : public MethodRequest {
        DataProcessor* processor_;
        ProductData data_;
        std::promise<bool> result_;
    public:
        ProcessProductRequest(DataProcessor* p, const ProductData& data)
            : processor_(p), data_(data) {}
        void execute() override {
            bool success = processor_->processProductImpl(data_);
            result_.set_value(success);
        }
        std::future<bool> getFuture() { return result_.get_future(); }
    };

    class ProcessOrderRequest : public MethodRequest {
        DataProcessor* processor_;
        OrderData data_;
        std::promise<bool> result_;
    public:
        ProcessOrderRequest(DataProcessor* p, const OrderData& data) : processor_(p), data_(data) {}
        void execute() override {
            bool success = processor_->processOrderImpl(data_);
            result_.set_value(success);
        }
        std::future<bool> getFuture() {
            return result_.get_future();
        }
    };

    bool processCustomerImpl(const CustomerData& data) { // Implementation methods (executed in the active object's thread)
        std::this_thread::sleep_for(std::chrono::milliseconds(500)); // Simulate work
        std::lock_guard<std::mutex> lock(data_mutex_);
        customers_[data.email] = data;
        std::cout << "[DataProcessor] Customer processed: " << data.name << " (" << data.email << ")" << std::endl;
        return true;
    }

    bool processProductImpl(const ProductData& data) {
        std::this_thread::sleep_for(std::chrono::milliseconds(300)); // Simulate work
        std::lock_guard<std::mutex> lock(data_mutex_);
        products_[data.productName] = data;
        std::cout << "[DataProcessor] Product processed: " << data.productName << " ($" << data.price << ")" << std::endl;
        return true;
    }

    bool processOrderImpl(const OrderData& data) {
        std::this_thread::sleep_for(std::chrono::milliseconds(800)); // Simulate work
        std::lock_guard<std::mutex> lock(data_mutex_);
        orders_[data.orderId] = data;
        std::cout << "[DataProcessor] Order processed: " << data.orderId << " (qty: " << data.quantity << ")" << std::endl;
        return true;
    }
public:
    DataProcessor() : worker_(&DataProcessor::run, this) {}
   ~DataProcessor() { shutdown(); }
    // *** Public interface methods - these are thread-safe ***
    std::future<bool> processCustomer(const CustomerData& data) {
        auto request = std::make_unique<ProcessCustomerRequest>(this, data);
        auto future = request->getFuture();
        queue_.enqueue(std::move(request));
        return future;
    }
    std::future<bool> processProduct(const ProductData& data) {
        auto request = std::make_unique<ProcessProductRequest>(this, data);
        auto future = request->getFuture();
        queue_.enqueue(std::move(request));
        return future;
    }
    std::future<bool> processOrder(const OrderData& data) {
        auto request = std::make_unique<ProcessOrderRequest>(this, data);
        auto future = request->getFuture();
        queue_.enqueue(std::move(request));
        return future;
    }
    void shutdown() {
        running_ = false;
        queue_.shutdown();
        if (worker_.joinable()) {
            worker_.join();
        }
    }
    // *** Query methods ***
    std::vector<std::string> getCustomerList() const {
        std::lock_guard<std::mutex> lock(data_mutex_);
        std::vector<std::string> result;
        for (const auto& pair : customers_) {
            result.push_back(pair.second.name + " (" + pair.first + ")");
        }
        return result;
    }
private:
    void run() {
        while (running_) {
            auto request = queue_.dequeue();
            if (request) {
                request->execute();
            }
        }
    }
};

class LogManager { // ACTIVE OBJECT 2: LOG MANAGER
    ActivationQueue queue_;
    std::thread worker_;
    std::atomic<bool> running_{true};
    std::vector<std::string> logs_;
    mutable std::mutex logs_mutex_;
    class LogRequest : public MethodRequest {
        LogManager* manager_;
        std::string message_;
        std::string level_;
    public:
        LogRequest(LogManager* m, const std::string& msg, const std::string& lvl)
            : manager_(m), message_(msg), level_(lvl) {}

        void execute() override {
            manager_->logImpl(message_, level_);
        }
    };
    class GetLogsRequest : public MethodRequest {
        LogManager* manager_;
        std::promise<std::vector<std::string>> result_;
    public:
        GetLogsRequest(LogManager* m) : manager_(m) {}
        void execute() override {
            auto logs = manager_->getLogsImpl();
            result_.set_value(logs);
        }
        std::future<std::vector<std::string>> getFuture() {
            return result_.get_future();
        }
    };
    void logImpl(const std::string& message, const std::string& level) {
        std::lock_guard<std::mutex> lock(logs_mutex_);
        auto now = std::chrono::system_clock::now();
        auto time_t = std::chrono::system_clock::to_time_t(now);
        std::string timestamp = std::to_string(time_t);
        std::string logEntry = "[" + timestamp + "] " + level + ": " + message;
        logs_.push_back(logEntry); std::cout << "[LogManager] " << logEntry << std::endl;
        if (logs_.size() > 100) { // Keep only last 100 logs
            logs_.erase(logs_.begin());
        }
    }
    std::vector<std::string> getLogsImpl() const {
        std::lock_guard<std::mutex> lock(logs_mutex_);
        return logs_;
    }
public:
    LogManager() : worker_(&LogManager::run, this) {}
    ~LogManager() { shutdown(); }
    void log(const std::string& message, const std::string& level = "INFO") {
        auto request = std::make_unique<LogRequest>(this, message, level);
        queue_.enqueue(std::move(request));
    }
    std::future<std::vector<std::string>>
    getLogs() {
        auto request = std::make_unique<GetLogsRequest>(this);
        auto future = request->getFuture();
        queue_.enqueue(std::move(request));
        return future;
    }
    void shutdown() {
        running_ = false;
        queue_.shutdown();
        if (worker_.joinable()) { worker_.join(); }
    }
private:
    void run() {
        while (running_) {
            auto request = queue_.dequeue();
            if (request) {
                request->execute();
            }
        }
    }
};

class CustomerForm {  // SIMPLE CONSOLE-BASED FORMS (simulating GUI input)
    DataProcessor       &processor_;
    LogManager          &logger_;
public:
    CustomerForm(DataProcessor & proc, LogManager & log) : processor_(proc), logger_(log) {}
    void display() {
        std::cout << "\n=== CUSTOMER REGISTRATION FORM ===" << std::endl;
        std::string name, email;
        int age{};
        std::cout << "Enter customer name: ";
        std::getline(std::cin, name);
        std::cout << "Enter customer email: ";
        std::getline(std::cin, email);
        std::cout << "Enter customer age: ";
        std::cin >> age;
        std::cin.ignore(); // consume newline
        CustomerData data{name, email, age};
        logger_.log("Customer form submitted: " + name);
        auto future = processor_.processCustomer(data); // Submit to active object asynchronously
        std::cout << "Processing customer data..." << std::endl;
        if (future.get()) { // Wait for result
            std::cout << "Customer registration successful!" << std::endl;
            logger_.log("Customer registered: " + name, "SUCCESS");
        } else {
            std::cout << "Customer registration failed!" << std::endl;
            logger_.log("Customer registration failed: " + name, "ERROR");
        }
    }
};

class ProductForm {
    DataProcessor&  processor_;
    LogManager&     logger_;
public:
    ProductForm(DataProcessor& proc, LogManager& log) : processor_(proc), logger_(log) {}
    void display() {
        std::cout << "\n=== PRODUCT REGISTRATION FORM ===" << std::endl;
        std::string name, category;
        double price{};
        std::cout << "Enter product name: ";
        std::getline(std::cin, name);
        std::cout << "Enter product category: ";
        std::getline(std::cin, category);
        std::cout << "Enter product price: $";
        std::cin >> price;
        std::cin.ignore(); // consume newline
        ProductData data(name, category, price);
        logger_.log("Product form submitted: " + name);
        auto future = processor_.processProduct(data); // Submit to active object asynchronously
        std::cout << "Processing product data..." << std::endl;
        // Wait for result
        if (future.get()) {
            std::cout << "Product registration successful!" << std::endl;
            logger_.log("Product registered: " + name, "SUCCESS");
        } else {
            std::cout << "Product registration failed!" << std::endl;
            logger_.log("Product registration failed: " + name, "ERROR");
        }
    }
};

class Application {
    DataProcessor processor_{};
    LogManager    logger_{};
    CustomerForm  customerForm_;  // TODO??: Why is {} rejected here?
    ProductForm   productForm_;   // TODO??: Why is {} rejected here?
public:
    Application() : customerForm_{processor_, logger_}, productForm_{processor_, logger_} {}
    void run() { logger_.log("Application started", "INFO");
        while (true) {
            displayMenu();
            int choice{};
            std::cin >> choice;
            std::cin.ignore(); // consume newline
            switch (choice) {
            case 1:
                customerForm_.display();
                break;
            case 2:
                productForm_.display();
                break;
            case 3:
                simulateOrder();
                break;
            case 4:
                displayCustomers();
                break;
            case 5:
                displayLogs();
                break;
            case 6:
                demonstrateConcurrency();
                break;
            case 0:
                logger_.log("Application shutting down", "INFO");
                break;
            default:
                std::cout << "Invalid choice!" << std::endl;
            }
        }
    }
private:
    void displayMenu() {
        std::cout << "\n=== MAIN MENU ===" << std::endl;
        std::cout << "1. Customer Registration Form" << std::endl;
        std::cout << "2. Product Registration Form" << std::endl;
        std::cout << "3. Simulate Order Processing" << std::endl;
        std::cout << "4. Display Customers" << std::endl;
        std::cout << "5. Display Logs" << std::endl;
        std::cout << "6. Demonstrate Concurrency" << std::endl;
        std::cout << "0. Exit" << std::endl; std::cout << "Choice: ";
    }
    void simulateOrder() {
        std::cout << "\n=== SIMULATING ORDER PROCESSING ===" << std::endl;
        OrderData order("ORD-001", "customer@example.com", "PROD-001", 5);
        logger_.log("Order processing started: " + order.orderId);
        auto future = processor_.processOrder(order);
        std::cout << "Processing order..." << std::endl;
        if (future.get()) {
            logger_.log("Order completed: " + order.orderId, "SUCCESS"); std::cout << "Order processed successfully!" << std::endl;
        } else {
            logger_.log("Order failed: " + order.orderId, "ERROR"); std::cout << "Order processing failed!" << std::endl;
        }
    }
    void displayCustomers() {
        std::cout << "\n=== REGISTERED CUSTOMERS ===" << std::endl;
        auto customers = processor_.getCustomerList();

        if (customers.empty()) {
            std::cout << "No customers registered yet." << std::endl;
        } else {
            for (const auto& customer : customers) {
                std::cout << "- " << customer << std::endl;
            }
        }
    }
    void displayLogs() {
        std::cout << "\n=== APPLICATION LOGS ===" << std::endl;
        auto logsFuture = logger_.getLogs();
        auto logs = logsFuture.get();

        if (logs.empty()) {
            std::cout << "No logs available." << std::endl;
        } else {
            for (const auto& log : logs) {
                std::cout << log << std::endl;
            }
        }
    }
    void demonstrateConcurrency() {
        std::cout << "\n=== CONCURRENCY DEMONSTRATION ===" << std::endl << "Submitting multiple requests simultaneously..." << std::endl;
        logger_.log("Concurrency test started", "INFO");

        std::vector<std::future<bool>> futures; // To submit multiple requests without waiting
        std::array<CustomerData,3> customers {{
            {"Carol Williams", "carol@example.com", 35},
            {"Alice Smith"s, "alice@example.com"s, 30},
            {"Bob Johnson"s, "bob@example.com"s, 25},
        }};
        std::array<ProductData,3> products {{
            {"Laptop", "Electronics", 999.99},
            {"Coffee Mug", "Kitchen", 15.99},
            {"Notebook", "Stationery", 5.99},
        }};
        for (const auto& customer : customers) { // Submit all requests asynchronously
            futures.push_back(processor_.processCustomer(customer));
        }
        for (const auto& product : products) {
            futures.push_back(processor_.processProduct(product));
        }
        std::cout << "All requests submitted above. Processing futures concurrently..." << std::endl;
        for (auto& future : futures) { // Wait for all to complete
            future.get();
        }
        std::cout << "All requests completed!" << std::endl; logger_.log("Concurrency test completed", "SUCCESS");
    }
};

int test1() {
    try {
        Application app;
        app.run();
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}
// To compile this program on Linux:
// g++ -std=c++11 -pthread -o active_object_demo active_object_demo.cpp
// KEY ACTIVE OBJECT PATTERN ELEMENTS DEMONSTRATED:
// 1. METHOD REQUEST: Base class for encapsulating method calls
// 2. ACTIVATION QUEUE: Thread-safe queue for method requests
// 3. SCHEDULER: Event loop in each active object's thread
// 4. SERVANT: Implementation of the actual business logic
// 5. PROXY: Public interface that converts method calls to requests
// 6. FUTURE: Asynchronous result handling
// This implementation shows how Active Objects provide:
// - Thread safety without explicit locking in client code
// - Asynchronous method execution
// - Decoupling of method invocation from execution
// - Clean separation of concerns
// - Better resource management than naked threads
} // END namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

namespace Example_grok { // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN derived from Sonnet LLM

template<typename T>
concept Executable = requires(T * t) {
    t->execute();
};

template<Executable T>
void safe_if_execute(T * request) {
    request->execute();
    delete request;
    // request = nullptr;
}

class Data_Model_Servant {
    std::string name{};
    int         age{};
    std::string address{};
public:
    void set_name(const std::string &n) {
        std::cout << "Setting name to " << n << "..." << std::endl;
        std::this_thread::sleep_for(std::chrono::seconds(2));
        name = n;
        std::cout << "Name set to " << n << std::endl;
    }
    void set_age(int a) {
        std::cout << "Setting age to " << a << "..." << std::endl;
        std::this_thread::sleep_for(std::chrono::seconds(2));
        age = a;
        std::cout << "Age set to " << a << std::endl;
    }
    void set_address(const std::string &addr) {
        std::cout << "Setting address to " << addr << "..." << std::endl;
        std::this_thread::sleep_for(std::chrono::seconds(2));
        address = addr;
        std::cout << "Address set to " << addr << std::endl;
    }
    std::tuple<std::string, int, std::string> get_all() {
        std::cout << "Getting all data..." << std::endl;
        std::this_thread::sleep_for(std::chrono::seconds(2));
        std::cout << "Data retrieved" << std::endl;
        return std::make_tuple(name, age, address);
    }
};

class MethodRequest {
public:
    //virtual ~MethodRequest() {};
    //virtual ~MethodRequest() = default;
    virtual ~MethodRequest() {};
    virtual void execute() = 0;
};

class SetNameRequest : public MethodRequest {
    Data_Model_Servant    *servant;
    std::string new_name;
public:
    SetNameRequest(Data_Model_Servant *s, const std::string &n) : servant{s}, new_name{n} {};
    void execute() final { servant->set_name(new_name); }
};

class SetAgeRequest : public MethodRequest {
    Data_Model_Servant *servant;
    int      new_age;
public:
    SetAgeRequest(Data_Model_Servant *s, int a) : servant(s), new_age(a) {}
    void execute() override { servant->set_age(new_age); }
};

class SetAddressRequest : public MethodRequest {
    Data_Model_Servant    *servant;
    std::string new_address;
public:
    SetAddressRequest(Data_Model_Servant *s, const std::string &addr) : servant(s), new_address(addr) {}
    void execute() override { servant->set_address(new_address); }
};

class GetAllRequest : public MethodRequest {
    Data_Model_Servant *servant;
public:
    GetAllRequest(Data_Model_Servant *s) : servant(s) {}
    void execute() override {
        auto data = servant->get_all();
        std::cout << "Current data: name=" << std::get<0>(data) << ", age=" << std::get<1>(data) << ", address=" << std::get<2>(data) << std::endl;
    }
};

class Scheduler_thread_run_instanciation {
    std::mutex                  mutex_for_queue_a_isrunning;
    std::atomic<bool>           atomic_is_running;
    std::queue<MethodRequest *> request_queue;
    std::thread                 worker_thread;
    std::condition_variable     cv;
public:
    Scheduler_thread_run_instanciation() : atomic_is_running{true}, worker_thread{ &Scheduler_thread_run_instanciation::run, this} {
        LOGGER_("$ A Scheduler has been" " CONstruted.")
    }
   ~Scheduler_thread_run_instanciation() {
        {   std::lock_guard<std::mutex> temp{mutex_for_queue_a_isrunning};
            atomic_is_running = false; }
        cv.notify_all();
        worker_thread.join();
        while(!request_queue.empty()) {
            delete request_queue.front();
            request_queue.pop();  // We have to pop, even though we deleted the element.
        }
        LOGGER_("$ A Scheduler has been DEstructed.")
    }
/*  Scheduler_thread_run_instanciation( Scheduler_thread_run_instanciation const & ) = delete;  // are these correct?
    Scheduler_thread_run_instanciation( Scheduler_thread_run_instanciation && ) = delete;
    Scheduler_thread_run_instanciation& operator=( Scheduler_thread_run_instanciation const & ) = delete;
    Scheduler_thread_run_instanciation& operator=( Scheduler_thread_run_instanciation && ) = delete; */
    void enqueue(MethodRequest *request) {
        {   std::lock_guard<std::mutex> temp{mutex_for_queue_a_isrunning};
            request_queue.push(request); }  // LIFO
        cv.notify_one();
    }
private:
    void run() {
        while(atomic_is_running) {
            MethodRequest *request{nullptr};  LOGGER_("$ TOP of run loop.");
            {   std::unique_lock<std::mutex> cv_wait_lock{mutex_for_queue_a_isrunning};  // why unique_lock?? recall: lock_guard, scope_lock<>
                //cv.wait(lock, [this] { return !request_queue.empty() || !atomic_is_running; });  // TODO??: Do we need this? Note mutex above.
                cv.wait( cv_wait_lock, [this] {
                    return not /*this->*/ request_queue.empty() || not atomic_is_running; }
                );  // TODO??: Do we need this? Note mutex above.
                // } TODO??: could we close the lock here instead?
                if( not atomic_is_running && request_queue.empty() ) { return; }
                if( not request_queue.empty()) {
                    request = request_queue.front();
                    request_queue.pop();
            }   }
            safe_if_execute(request);  // if( request ) { request->execute(); delete request; }
        }
    }
};


class ActiveObject {
    Data_Model_Servant   servant{};
    Scheduler_thread_run_instanciation scheduler{};
public:
    void set_name(const std::string &n) {
        MethodRequest *request = new SetNameRequest(&servant, n);
        scheduler.enqueue(request);
    }
    void set_age(int a) {
        MethodRequest *request = new SetAgeRequest(&servant, a);
        scheduler.enqueue(request);
    }
    void set_address(const std::string &addr) {
        MethodRequest *request = new SetAddressRequest(&servant, addr);
        scheduler.enqueue(request);
    }
    void get_all() {
        MethodRequest *request = new GetAllRequest(&servant);
        scheduler.enqueue(request);
    }
};

void test1() {
    ActiveObject      ao{};
    std::atomic<bool> running{true};
    std::jthread      looping_output_thread{ // CHILD THREAD for periodic get_all/pring requests
        [&ao, &running]() {
            while(running) {
                std::this_thread::sleep_for(std::chrono::seconds(5));
                if(running) { ao.get_all(); }
            }
        }
    };

    std::string command;  // MAIN THREAD for user input
    while(running) {
        std::cout << "Enter command: ";
        if(std::getline(std::cin, command)) {
            if(command == "exit") {
                running = false;
            } else if(command.substr(0, 9) == "set_name ") {
                std::string name = command.substr(9);
                ao.set_name(name);
            } else if(command.substr(0, 8) == "set_age ") {
                try {
                    int age = std::stoi(command.substr(8));
                    ao.set_age(age);
                } catch(const std::exception &) { std::cout << "Invalid age" << std::endl; }
            } else if(command.substr(0, 12) == "set_address ") {
                std::string address = command.substr(12);
                ao.set_address(address);
            } else if(command == "get_all") {
                ao.get_all();
            } else {
                std::cout << "Unknown command" << std::endl;
            }
        }
    }
    looping_output_thread.join(); // TODO??: why do this? looping_output_thread.detach();
};
} // END namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

int main(int const argc, char const *arv[]) { string my_arv{ *arv }; cout << "$$ my_this: argc, argv:" << argc << "," << my_arv << "." << endl;
    cin.exceptions(std::istream::failbit);
    // Detail::crash_signals_register();
/*  Prompt LLM: Code an example Linux C++ application with 2 input forms and 3 data fields, to fully demonstrate the Active Object design pattern re: Active Object: An Object Behavioral Pattern for Concurrent Programming - Lavender and Schmidt; and re: Prefer Using Active Objects Instead of Naked threads - Herb Sutter. */
    Example_sonnet::test1();
    //Example_grok::test1();
    cout << "###" << endl;
    return EXIT_SUCCESS;
}
