
void Optimizer::diagnostic_tree(int iteration) {
    json tracer = json::object();
    tracer["directed"] = true;
    tracer["multigraph"] = false;
    tracer["graph"] = json::object();
    tracer["graph"]["name"] = "GOSDT Trace";
    tracer["links"] = json::array();
    tracer["nodes"] = json::array();
    diagnostic_tree(this -> root, tracer);

    int indentation = 2;

    std::stringstream trace_name;
    trace_name << Configuration::tree << "/" << iteration << ".gml";
    std::string trace_result = tracer.dump(indentation);
    std::ofstream out(trace_name.str());
    out << trace_result;
    out.close();

    return;
}
bool Optimizer::diagnostic_tree(key_type const & identifier, json & tracer) {
    auto task_accessor = State::graph.vertices.find(identifier);
    if (task_accessor == State::graph.vertices.end()) { return false; }
    Task & task = task_accessor -> second;

    json node = json::object();
    node["id"] = identifier.to_string();
    node["capture"] = task.capture_set().to_string();
    node["support"] = task.support();
    node["terminal"] = task.lowerbound() == task.upperbound();

    
    if (task.lowerbound() == task.base_objective()) { 
        tracer["nodes"].push_back(node);
        return true;
    }

    json scores = json::object();

    unsigned int m = State::dataset.width();
    unsigned int k = 0;
    float score_k = std::numeric_limits<float>::max();

    auto bounds = State::graph.bounds.find(task.identifier());
    if (bounds == State::graph.bounds.end()) { return true; }
    for (bound_iterator iterator = bounds -> second.begin(); iterator != bounds -> second.end(); ++iterator) {
        int feature = std::get<0>(* iterator);

        std::string type, relation, reference;
        State::dataset.encoder.encoding(feature, type, relation, reference);
        float upper = std::get<2>(* iterator);
        scores[reference] = upper;
        if (upper < score_k) {
            score_k = upper;
            k = feature;
        }
    }
    unsigned int decoded_index;
    std::string type, relation, reference;
    State::dataset.encoder.decode(k, & decoded_index);
    State::dataset.encoder.encoding(k, type, relation, reference);
    node["threshold"] = reference;
    node["scores"] = scores;
    tracer["nodes"].push_back(node);
    if (score_k < std::numeric_limits<float>::max()) {
        auto left_key = State::graph.children.find(std::make_pair(identifier, -(k + 1)));
        if (left_key != State::graph.children.end()) {    
            diagnostic_tree(left_key -> second, tracer);
        }
        auto right_key = State::graph.children.find(std::make_pair(identifier, k + 1));
        if (right_key != State::graph.children.end()) {
            diagnostic_tree(right_key -> second, tracer);
        }
    }

    return true;
}
