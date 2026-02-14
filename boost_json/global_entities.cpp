/** Copyright (c) Grant Rostig, grantrostig.com, Boost 1.0 license
    Snippet
*/
#include "global_entities.hpp"
//#include <gsl/gsl>      // $ sudo dnf install guidelines-support-library-devel
//#include <bits/stdc++.h>
#include <chrono>
#include <iostream>
#include <map>
#include <source_location>
#include <stacktrace>
#include <string>
#include <cassert>
#include <csignal>
#include <cstring>  // only needed here, for strsignal().

using std::cin; using std::cout; using std::cerr; using std::clog; using std::endl; using std::string;  // NOT using namespace std;
using namespace std::string_literals;  // Doesn't cause harm?
using namespace std::chrono_literals;  // Doesn't cause harm?

namespace Detail {  // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
/** signals that cause "terminate" and sometimes "core dump"  https://en.wikipedia.org/wiki/Signal_(IPC)
   We define here all the signal names listed in POSIX (1003.1-2008);
   as of 1003.1-2013, no additional signals have been added by POSIX.
   We also define here signal names that historically exist in every
   real-world POSIX variant (e.g. SIGWINCH).
   Signals in the 1-15 range are defined with their historical numbers.
   For other signals, we use the BSD numbers.
   There are two unallocated signal numbers in the 1-31 range: 7 and 29.
   Signal number 0 is reserved for use as kill(pid, 0), to test whether
   a process exists without sending it a signal.
 ISO C99 signals.
#define	SIGINT		2	 Interactive attention signal.
#define	SIGILL		4	 Illegal instruction.
#define	SIGABRT		6	 Abnormal termination.
#define	SIGFPE		8	 Erroneous arithmetic operation.
#define	SIGSEGV		11	 Invalid access to storage.
#define	SIGTERM		15	 Termination request.
 Historical signals specified by POSIX.
#define	SIGHUP		1	 Hangup.
#define	SIGQUIT		3	 Quit.
#define	SIGTRAP		5	 Trace/breakpoint trap.
#define	SIGKILL		9	 Killed.
#define	SIGPIPE		13	 Broken pipe.
#define	SIGALRM		14	 Alarm clock.
 Adjustments and additions to the signal number constants for most Linux systems.
#define SIGSTKFLT	16	 Stack fault (obsolete).
#define SIGPWR		30	 Power failure imminent.
 Historical signals specified by POSIX.
#define SIGBUS		 7	 Bus error.
#define SIGSYS		31	 Bad system call.
 New(er) POSIX signals (1003.1-2008, 1003.1-2013).
#define SIGURG		23	 Urgent data is available at a socket.
#define SIGSTOP		19	 Stop, unblockable.
#define SIGTSTP		20	 Keyboard stop.
#define SIGCONT		18	 Continue.
#define SIGCHLD		17	 Child terminated or stopped.
#define SIGTTIN		21	 Background read from control terminal.
#define SIGTTOU		22	 Background write to control terminal.
#define SIGPOLL		29	 Pollable event occurred (System V).
#define SIGXFSZ		25	 File size limit exceeded.
#define SIGXCPU		24	 CPU time limit exceeded.
#define SIGVTALRM	26	 Virtual timer expired.
#define SIGPROF		27	 Profiling timer expired.
#define SIGUSR1		10	 User-defined signal 1.
#define SIGUSR2		12	 User-defined signal 2.
 Nonstandard signals found in all modern POSIX systems
   (including both BSD and Linux).
#define SIGWINCH	28	 Window size change (4.3 BSD, Sun).
 Archaic names for compatibility.
#define SIGIO		SIGPOLL	 I/O now possible (4.2 BSD).
#define SIGIOT		SIGABRT	 IOT instruction, abort() on a PDP-11.
#define SIGCLD		SIGCHLD	 Old System V name
#define __SIGRTMIN	32
#define __SIGRTMAX	64
*/

std::map<int, std::string> signal_info{  // Yes some of these entries are overkill for current use, but a good reference for linux.
    {SIGABRT, "SIGABRT"},
    {SIGALRM, "SIGALRM"},
    {SIGBUS, "SIGBUS"},
    {SIGCHLD, "SIGCHLD"},
    {SIGCONT, "SIGCONT"},
    {SIGFPE, "SIGFPE"},
    {SIGHUP, "SIGHUP"},
    {SIGILL, "SIGILL"},
    {SIGINT, "SIGINT"},
    {SIGIO, "SIGIO"},
    {SIGKILL, "SIGKILL"},
    {SIGPIPE, "SIGPIPE"},
#ifdef SIGPOLL
    {SIGPOLL, "SIGPOLL"},
#endif
    {SIGPROF, "SIGPROF"},
    {SIGQUIT, "SIGQUIT"},
    {SIGSEGV, "SIGSEGV"},
    {SIGSTOP, "SIGSTOP"},
    {SIGSYS, "SIGSYS"},
    {SIGTERM, "SIGTERM"},
    {SIGTRAP, "SIGTRAP"},
    {SIGTSTP, "SIGTSTP"},
    {SIGTTIN, "SIGTTIN"},
    {SIGTTOU, "SIGTTOU"},
    {SIGURG, "SIGURG"},
    {SIGUSR1, "SIGUSR1"},
    {SIGUSR2, "SIGUSR2"},
    {SIGVTALRM, "SIGVTALRM"},
    {SIGWINCH, "SIGWINCH"},
    {SIGXCPU, "SIGXCPU"},
    {SIGXFSZ, "SIGXFSZ"}
};

/// Gives a source location for printing.  Used for debugging.
std::string
source_loc() {
    using loc = std::source_location;
    //using ts = std::to_string;  // TODO??: why does this not work?  alternative approach?
    std::string result { "\n"s+loc::current().file_name() +":"s
                              +std::to_string(loc::current().line())
                              +std::to_string(loc::current().column())+"]`"s
                              +loc::current().function_name()+"`."s};
    return result;
}

/** Called as we try to ABORT, after already getting another signal from the application code.
    OR TODO??: should we exit() or terminate() and not use this function? */
auto crash_tracer_SIGABRT(int signal_number) -> void {
    (void)signal_number;
    /* Other ways to suppress unused warning:
        int __attribute__((unused)) myVariable = 42; //gcc only      ///OR///
        #pragma GCC diagnostic push
        #pragma GCC diagnostic ignored "-Wunused-variable"
        int myVariable = 42; // Warning is suppressed for this line
        #pragma GCC diagnostic pop
    */
    //LOGGERX( "$$ We are getting out and that generated another signal, presumably ABRT, we got signal number", signal_number);
    //LOGGERX( "$$ IGNORE THIS ONE, just shown for understanding:", std::stacktrace::current());
    LOGGER_( "$$ SIGABRT called by crash_tracer, or it occurred in code. Now we are at the very end of crash_tracer_SIGABRT!");
    return;  // TODO??: I think ABRT now, after this return it actually aborts and is done and overwith.
}

/** Prints a stack trace of crash location.  Used for debugging. Note funny code is if tty is in RAW or CBREAK mode */
auto crash_tracer(int signal_number) -> void {  // Note std::signal wants an int parameter, so we have one here too for symmetry.
    std::string signal_defined_symbol{"Unknown defined symbol for signal number."};
    if ( auto it = signal_info.find(signal_number); it != signal_info.end())
        signal_defined_symbol=it->second;
    //char * char_ptr{};
    //psignal(signal_number, char_ptr );
    //psiginfo(my_siginfo, char_ptr );
    std::string signal_explantion{strsignal(signal_number)};
    cerr << "\n\r$$ :::CRASH_ERROR:::Got signal number:" << signal_number<<", "<< signal_defined_symbol <<", "<< signal_explantion <<".";
    cerr << "\n\r$$ std::stack_trace::current():" << std::stacktrace::current() <<":::END_ERROR_tracer."<<endl;

    if ( bool is_want_prompt{true}; is_want_prompt ) {  // might want this variable in future, and reminds of new if feature.
        std::string reply; cout << "\n\r$$ CRASH_ERROR: 'q' for exit(1) or inlikely: CR to continue:"; cout.flush(); cin.clear();
        getline( cin, reply );
        if ( reply == "q" )
            // gcc exit(): Call all functions registered with `atexit' and `on_exit',
            // in the reverse of the order in which they were registered,
            // perform stdio cleanup, and terminate program execution with STATUS.  */
            exit(signal_number);  // TODO??: OR alternatively? $ std::exit(42) or teminate, or quick_exit()??
        else
            return;  // TODO??: return continues to where? OR alternatively? $std::exit(42) or teminate, or quick_exit()??
    }
    else {
        /* Debugging code
        using namespace std::chrono_literals;
        std::chrono::duration my_duration = 2s;
        cerr << "Wait time:" << 2s << " + in variable:"<< my_duration <<endl;
        std::this_thread::sleep_for( my_duration ); // sleep(2);  // seconds
        std::this_thread::sleep_for( std::chrono_literals::s::1 ); //todo??: how to do this?
        LOGGER_LOC( my_tracer(): Now- after stacktrace documenting a catastrophic error- we need to abort().);
        */

        //std::signal( SIGABRT, SIG_DFL );  // TODO??: repair normal signals in preparation for abort() // OR maybe?? $ std::signal( SIGABRT, SIG_IGN );

        // gcc abort(): Abort execution and generate a core-dump.  */
        std::abort();  // TODO??: OR alternatively? $ std::exit(42) or teminate, or quick_exit()??
        // *** we never get here, right?
        assert( false && "Doing something in user-space after std::abort() is ERROR.\n" );
        return;
    }
}

/// Enables printing of stack dump on crash.  Used for debugging. */
/// signals that cause "terminate" of a process and sometimes "core dump"  https://en.wikipedia.org/wiki/Signal_(IPC)
auto crash_signals_register() -> void {
#ifdef SIGRTMIN  // Yes this is overkill for current use, but a good reference.
    for (int signal_number{SIGRTMIN}; signal_number <= SIGRTMAX; ++signal_number)
        signal_info[signal_number] = "SIGRT" + std::to_string(signal_number - SIGRTMIN);
#endif
    std::signal( SIGABRT, crash_tracer_SIGABRT );  // We really needed special handling for this signal since we wanted to allow crash_tracer to abort()?  Don't remember.
    std::signal( SIGBUS,  crash_tracer );
    std::signal( SIGFPE,  crash_tracer );
    std::signal( SIGHUP,  crash_tracer );
    std::signal( SIGINT,  crash_tracer );
    std::signal( SIGILL,  crash_tracer );
  //std::signal( SIGKILL, crash_tracer );  // can't be caught?
    std::signal( SIGQUIT, crash_tracer );
    std::signal( SIGSEGV, crash_tracer );
  //std::signal( SIGSTOP, crash_tracer );  // can't be caught?
    std::signal( SIGSYS,  crash_tracer );
  //std::signal( SIGTRAP, crash_tracer );  // usually a debugger or breakpoint
    std::signal( SIGXCPU, crash_tracer );
    std::signal( SIGXFSZ, crash_tracer );
}
} // End Namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
