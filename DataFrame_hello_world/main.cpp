/* edited by: Grant Rostig
   See also: DataFrame: https://htmlpreview.github.io/?https://github.com/hosseinmoein/DataFrame/blob/master/docs/HTML/DataFrame.html

Copyright (c) 2019-2026, Hossein Moein
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
conditions are met:
* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
disclaimer in the documentation and/or other materials provided with the distribution.
* Neither the name of Hossein Moein and/or the DataFrame nor the names of its contributors may be used to endorse or promote
products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
EVENT SHALL Hossein Moein BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
DAMAGE.
*/
#include <DataFrame/DataFrame.h>                   // Main DataFrame header
#include <DataFrame/DataFrameFinancialVisitors.h>  // Financial algorithms
#include <DataFrame/DataFrameMLVisitors.h>         // Machine-learning algorithms
#include <DataFrame/DataFrameStatsVisitors.h>      // Statistical algorithms
#include <DataFrame/Utils/DateTime.h>              // Cool and handy date-time object
#include <iostream>
using namespace hmdf;                              // DataFrame library is entirely under hmdf name-space
using ULDataFrame = StdDataFrame<unsigned long>;
using StrDataFrame = StdDataFrame<std::string>;
using DTDataFrame = StdDataFrame<DateTime>;        // A DataFrame with DateTime index type
struct  MyData  {
    int         i { 10 };
    double      d { 5.5 };
    std::string s { "Some Arbitrary String" };
    MyData() = default;
};
int main(int, char *[])  {
    ThreadGranularity::set_optimum_thread_level();
    std::vector<unsigned long>  idx_col1 = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    std::vector<MyData>         mydata_col (10);
    std::vector<int>            int_col1 = { 1, 2, -3, -4, 5, 6, 7, 8, 9, -10 };
    std::vector<double>         dbl_col1 = { 0.01, 0.02, 0.03, 0.03, 0.05, 0.06, 0.03, 0.08, 0.09, 0.03 };
    ULDataFrame ul_df1;
    ul_df1.load_index(std::move(idx_col1));
    ul_df1.load_column("dbl_col", std::move(dbl_col1));
    ul_df1.load_column("my_data_col", std::move(mydata_col));
    ul_df1.load_column("integers", std::move(int_col1));

    std::vector<unsigned long>  idx_col2 = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    std::vector<std::string>    str_col = { "A", "B", "C", "D", "E", "F", "G", "H", "I", "J" };
    std::vector<std::string>    cool_col = { "Azadi", "Hello", " World", "!", "Hype", "cubic spline", "Shawshank", "Silverado", "Arash", "Pardis" };
    std::vector<double>         dbl_col2 = { 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0 };

    ULDataFrame ul_df2;             // Also, you can load data into a DataFrame all at once. In this case again the data is moved to the DataFrame.
    ul_df2.load_data(std::move(idx_col2),
                     std::make_pair("string col",  str_col),
                     std::make_pair("Cool Column", cool_col),
                     std::make_pair("numbers",     dbl_col2));

    const auto &cool_col_ref = ul_df2.get_column<std::string>("Cool Column");  // get_column(): involves 1 or sometimes 2 hash-table lookups. So, you should not call it repeatedly in a loop. Instead get a reference to it and use the reference.
    const auto &str_col_ref =  ul_df2.get_column<std::string>("string col");
    std::cout << "$$ cool_col_ref[1] , cool_col_ref[2] , cool_col_ref[3] = "; std::cout << cool_col_ref[1] << cool_col_ref[2] << cool_col_ref[3] << std::endl;
    std::cout << "$$ Str Column = "; for (const auto &str : str_col_ref) std::cout << str << ", "; std::cout << std::endl;

    std::cout << "$$ ul_df2.write<std::ostream, std::string, double>(std::cout, io_format::csv2) :\n$$";  // You can write the data to a file or stdout in a few formats. You must specify all the column types, but only once.
    ul_df2.write<std::ostream, std::string, double>(std::cout, io_format::csv2); std::cout<<" :\n$$";    // You can write the data to a file or stdout in a few formats. You must specify all the column types, but only once.

    StrDataFrame ibm_df; // Also, you can load data into a DataFrame from a file, supporting a few different formats.
    ibm_df.read("IBM.csv", io_format::csv2);
           // To access a column, you must know its name (or index) and its type. In case of a "standard" DataFrame (not a view),
           // the columns are returned as a reference to a std::vector of type of that column.
    std::cout << "$$ There are " << ibm_df.get_column<double>("IBM_Close").size() << " IBM close prices." << std::endl;
    std::cout << "$$ There are " << ibm_df.get_index().size() << " IBM indices." << std::endl;

    ibm_df.write<double, long>("/tmp/test.json", io_format::json);
 // One can serialize and deserialize the DataFrame both in string and binary formats. This could be used to transmit a DataFrame from one node to another or store a DataFrame in databases, caches, ...
    const std::string ibm_df_serialized = ibm_df.serialize<double, long>();
    StrDataFrame ibm_df_2;
    ibm_df_2.deserialize(ibm_df_serialized);

    using ul_idx_t = ULDataFrame::IndexType;  // unsigned long.
           // You can sort by one or multiple columns. You must specify all the column types, but only once.
           // Sort first by the index column in ascending order then by "string col" column in descending order.
    ul_df2.sort<ul_idx_t, std::string, double, std::string>(DF_INDEX_COL_NAME, sort_spec::ascen, "string col", sort_spec::desce);

    auto    above_150_fn = [](const std::string &, const double &val)-> bool { return (val > 150.0); };
    auto    above_150_df = ibm_df.get_data_by_sel<double, decltype(above_150_fn), double, long>("IBM_Close", above_150_fn); // Get DataFrame by selecting on one or multiple columns. One must specify all the column types, but only once.
    auto    above_150_view = ibm_df.get_view_by_sel<double, decltype(above_150_fn), double, long>("IBM_Close", above_150_fn); // Or, a view.

 // Get another DataFrame by group-bying on one or multiple columns. You must specify only the type(s) of column(s) you are group-bying.
 //   Group-by column: dbl_col, and specifying how to summarize the index column and each of the other columns.
    auto    gb_df = ul_df1.groupby1<double>("dbl_col",
                                             LastVisitor<ul_idx_t, ul_idx_t>(),
                                             std::make_tuple("integers",    "sum_int",      SumVisitor<int>()),
                                             std::make_tuple("my_data_col", "last_my_data", LastVisitor<MyData>()));

 // Run statistical, financial, ML,+ algorithms on one or multiple columns by using visitors. You must specify the column(s) type(s).
 // TODO??: what does this mean?: The visitor's data column is of type double and its index column is of type std::string.
    StdVisitor<double, std::string> stdev_v;
    ibm_df.visit<double>("IBM_Close", stdev_v);
    std::cout << "$$ Standard deviation of IBM close prices: " << stdev_v.get_result() << std::endl;

    DTDataFrame ibm_dt_df;  // DataFrames with index type of DateTime which is a handy object for date/time manipulations.
    ibm_dt_df.read( "DT_IBM.csv",  io_format::csv2);  // Read the AAPL and IBM market data. Data for these two stocks start and end at different dates. But there is overlapping data between them.
    ibm_dt_df.fill_missing<double>({ "IBM_Close", "IBM_Open", "IBM_High", "IBM_Low" }, fill_policy::linear_interpolate); // First let’s make sure if there are missing data in our important columns, we fill them up.
    DTDataFrame aapl_dt_df;
    aapl_dt_df.read("DT_AAPL.csv", io_format::csv2);
                                    // TODO??: Do this for aapl too?: ibm_dt_df.fill_missing<double>({ "IBM_Close", "IBM_Open", "IBM_High", "IBM_Low" }, fill_policy::linear_interpolate); // First let’s make sure if there are missing data in our important columns, we fill them up.
    DTDataFrame aapl_ibm = ibm_dt_df.join_by_index<DTDataFrame, double, long>(aapl_dt_df, join_policy::inner_join); // Now we join the AAPL and IBM DataFrames using their indices and applying inner-join policy.
 // Now we calculate the Pearson correlation coefficient between AAPL and IBM close prices. The visitor's data columns are of type double and its index column is of type DateTime.
    CorrVisitor<double, DateTime> corrl_v;
    std::cout << "$$ Correlation between AAPL and IBM close prices: "
              << aapl_ibm.visit<double, double>("AAPL_Close", "IBM_Close", corrl_v).get_result() << std::endl;

 // Now let’s do something more sophisticated and calculate rolling exponentially weighted correlations between IBM and Apple close prices.
 //   Since this is a rolling, moving, analysis, the result is a vector of exponentially weighted correlations for each date in the data stream.
    ewm_corr_v<double> ewmcorr { exponential_decay_spec::span, 3 };
    const auto        &ewmcorr_result = aapl_ibm.single_act_visit<double, double>("AAPL_Close", "IBM_Close", ewmcorr).get_result();
    std::cout << "$$ The last exponentailly weighted correlation between AAPL and IBM close prices: " << ewmcorr_result.back() << std::endl;

    using dt_idx_t = DTDataFrame::IndexType;  // This is just DateTime.
    DTDataFrame aapl_ohlc =                   // Apple data is daily. Let’s create 10-day OHLC (plus a bunch of other stats) for close prices.
        aapl_dt_df.bucketize(
            bucket_type::by_count,
            10,
            LastVisitor<dt_idx_t, dt_idx_t>(),  // How to bucketize the index column
            std::make_tuple("AAPL_Close",  "Open",          FirstVisitor<double, dt_idx_t>()),
            std::make_tuple("AAPL_Close",  "High",          MaxVisitor<double, dt_idx_t>()),
            std::make_tuple("AAPL_Close",  "Low",           MinVisitor<double, dt_idx_t>()),
            std::make_tuple("AAPL_Close",  "Close",         LastVisitor<double, dt_idx_t>()),
            std::make_tuple("AAPL_Close",  "Mean",          MeanVisitor<double, dt_idx_t>()),
            std::make_tuple("AAPL_Close",  "Median",        MedianVisitor<double, dt_idx_t>()),
            std::make_tuple("AAPL_Close",  "25% Quantile",  QuantileVisitor<double, dt_idx_t>(0.25)),
            std::make_tuple("AAPL_Close",  "Std",           StdVisitor<double, dt_idx_t>()),
                                        // "Mode" column is a column of std::array<ModeVisitor::DataItem, 2>'s
            std::make_tuple("AAPL_Close",  "Mode",          ModeVisitor<2, double, dt_idx_t>()),
            std::make_tuple("AAPL_Close",  "MAD",           MADVisitor<double, dt_idx_t>(mad_type::mean_abs_dev_around_mean)),
                                        // "Z Score" column is a column of std::vector<double>'s
            std::make_tuple("AAPL_Close",  "Z Score",       ZScoreVisitor<double, dt_idx_t>()),
                                        // "Return Vector" column is a column of std::vector<double>'s
            std::make_tuple("AAPL_Close",  "Return Vector", ReturnVisitor<double, dt_idx_t>(return_policy::log)),
            std::make_tuple("AAPL_Volume", "Volume",        SumVisitor<long, dt_idx_t>()));
    std::cout <<"$$ aapl_ohlc.write<std::ostream, double, long, std::vector<double>>(std::cout, io_format::csv2);\n$$Warning: large amount of output."<<std::endl;
    aapl_ohlc.write<std::ostream, double, long, std::vector<double>>(std::cout, io_format::csv2); std::cout<<std::endl; // Warning: large amount of output
    auto    random_view = aapl_dt_df.get_view_by_rand<double, long>(random_policy::frac_rows_no_seed, 0.35); // Get a view of a random sample of appel data. We randomly sample 35% of the data.

 // Now let’s do some stuff that are a little more involved (multi steps). There are a lot of theories,
 // math, and procedures that I am skipping to explain here. See docs for more details.
 // NOTE: I am applying the following analysis to financial data but it equally applies to other scientific fields.
 // Let’s calculate IBM daily returns and then try to find clusters of similar patterns in those returns.
 // We will use k-means clustering to do that.
    ReturnVisitor<double>   return_v { return_policy::log };
 // Calculate the returns and load them as a column.
    ibm_dt_df.load_result_as_column<double>("IBM_Close", std::move(return_v), "IBM_Return");
    ibm_dt_df.get_column<double>("IBM_Return")[0] = 0;                      // Remove the NaN. It messes things up.

 // Try to find 4 clusters.
    KMeansVisitor<4, double, DateTime> kmeans_v { 1000 };  // Iterate at most 1000 times.
    ibm_dt_df.single_act_visit<double>("IBM_Return", kmeans_v);

    const auto &cluster_means = kmeans_v.get_result();
    std::cout << "$$ Means of clusters are: "; for (const auto &mean : cluster_means) std::cout << mean << ", "; std::cout << std::endl;
    std::cout << "\n$$ Clusters are: ";                     // Warning: This produces a very large output.
    for (const auto &mean1 : kmeans_v.get_clusters()) {
        for (const auto &mean2 : mean1)
            std::cout << mean2 << ", ";
        std::cout << ".\n" << std::endl;
    }

 // Find a few quantiles of IBM returns.
    QuantileVisitor<double, DateTime> qt50 { 0.5,  quantile_policy::mid_point };  // 50%
    QuantileVisitor<double, DateTime> qt75 { 0.75, quantile_policy::mid_point };  // 75%
    QuantileVisitor<double, DateTime> qt95 { 0.95, quantile_policy::mid_point };  // 95%
    ibm_dt_df.single_act_visit<double>("IBM_Return", qt50);
    ibm_dt_df.single_act_visit<double>("IBM_Return", qt75);
    ibm_dt_df.single_act_visit<double>("IBM_Return", qt95);
    std::cout << "$$IBM returns 50% quantile: " << qt50.get_result() << ", " << "75% quantile: " << qt75.get_result() << ", " << "95% quantile: " << qt95.get_result() << std::endl;

 // Now let’s do another interesting thing. Let’s take the IBM returns curve and split it into 3 different curves;
 //   Trend, Seasonal, and Idiocentric or Residual or Random.
 //   For the sake of this exercise, we assume IBM business goes through 170-day seasonal cycles.
    DecomposeVisitor<double, DateTime>  decom { 170, 0.6, 0.01 };
    ibm_dt_df.single_act_visit<double>("IBM_Return", decom); // After this call, the 3 curves will be in decom visitor instance. See docs how to get them and analyze them.

 // But what if you don’t know the seasonality of IBM returns which would be most of the time. No worries, // Mr. Joseph Fourier comes to the rescue.
    FastFourierTransVisitor<double, DateTime> fft;
    ibm_dt_df.single_act_visit<double>("IBM_Return", fft);
    const auto &magnitudes = fft.get_magnitude();
    double      max_val = 0;

 // The following analysis and conclusion are over simplified and naive. It is more involved which is behind the scope of Hello World. But this is the basic idea.
    for (std::size_t i = 1; i < magnitudes.size(); ++i)  {
        const double val = 1.0 / magnitudes[i];
        if (val > max_val) max_val = val;
    }
    std::cout << "$$The seasonality of IBM returns is " << std::size_t(max_val) << " days.\n"
              << "So use this instead of 170 days in decomposition analysis." << std::endl;

 // Use lagged auto-correlation to verify your finding.
    FixedAutoCorrVisitor<double, DateTime> facorr  { 170,                  roll_policy::blocks };
    FixedAutoCorrVisitor<double, DateTime> facorr2 { std::size_t(max_val), roll_policy::blocks };
    ibm_dt_df.single_act_visit<double>("IBM_Return", facorr);
    ibm_dt_df.single_act_visit<double>("IBM_Return", facorr2);
    std::cout << "Auto correlations of 170 days lag: ";
    for (std::size_t i = facorr.get_result().size() - 1; i > facorr.get_result().size() - 10; --i)
        std::cout << facorr.get_result()[i] << ", ";
    std::cout << std::endl;
    std::cout << "Auto correlations of " << std::size_t(max_val) << " days lag: ";
    for (std::size_t i = facorr2.get_result().size() - 1; i > facorr2.get_result().size() - 10; --i)
        std::cout << facorr2.get_result()[i] << ", ";
    std::cout << std::endl;
    return (0);
}
