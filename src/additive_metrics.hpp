#ifndef ADDITIVE_METRICS_H
#define ADDITIVE_METRICS_H
#include <cstddef>
#include <functional>
#include <tuple>

class State; // Forward declaration for State reference parameter

struct Objective {
    Objective(const int falses, const int regularization, State & state);
    Objective() = default;

    int falses;
    int regularization;
    float objective;

    // Note: operator+ creates new Objective but doesn't have State
    // This is used in contexts where State isn't available
    // The objective value is already calculated, so we can create with default constructor
    // and set objective directly, or we need to refactor to store mismatch_cost
    Objective operator+(const Objective &other) const {
        Objective result;
        result.falses = falses + other.falses;
        result.regularization = regularization + other.regularization;
        result.objective = objective + other.objective; // Use pre-calculated values
        return result;
    }

    std::tuple<float, int, int> to_tuple() const {
        return std::make_tuple(objective, falses, regularization);
    }

    bool operator==(const Objective &other) const {
        return objective == other.objective;
    }

    bool operator<(const Objective &other) const {
        return objective < other.objective;
    }

    bool operator<(const float &other) const { return objective < other; }
    bool operator<=(const float &other) const { return objective <= other; }
    bool operator>(const float &other) const { return objective > other; }
    bool operator>=(const float &other) const { return objective >= other; }
};

struct ValuesOfInterest {
    ValuesOfInterest() = default;
    ValuesOfInterest(const int TP, const int TN, const int regularization)
        : TP(TP), TN(TN), regularization(regularization){};

    int TP;
    int TN;
    int regularization;

    ValuesOfInterest operator+(const ValuesOfInterest &other) const {
        return ValuesOfInterest(TP + other.TP, TN + other.TN,
                                regularization + other.regularization);
    }

    bool operator==(const ValuesOfInterest &other) const {
        return TP == other.TP && TN == other.TN &&
               regularization == other.regularization;
    }

    size_t hash() const {
        size_t seed = 0;
        // boost::hash_combine(result, TP);
        // boost::hash_combine(result, TN);
        // boost::hash_combine(result, regularization);
        seed ^= TP + 0x9e3779b9 + (seed << 6) + (seed >> 2);
        seed ^= TN + 0x9e3779b9 + (seed << 6) + (seed >> 2);
        seed ^= regularization + 0x9e3779b9 + (seed << 6) + (seed >> 2);
        return seed;
    }

    std::tuple<int, int, int> to_tuple() const {
        return std::make_tuple(TP, TN, regularization);
    }
};

struct ObjectiveHash {
    std::size_t operator()(const Objective &k) const {
        return std::hash<float>{}(k.objective);
    }
};

struct ObjectiveLess {
    bool operator()(const Objective &left, const Objective &right) const {
        return left.objective < right.objective;
    }
};
#endif