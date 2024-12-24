package info.openmeta.starter.flow.node;

import info.openmeta.starter.flow.enums.NodeExceptionSignal;
import lombok.Data;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Node context environment variables, including input data,
 * and the {actionCode: returnValue} pair after each FlowNode execution
 */
@Data
public class NodeContext {

    private final Map<String, Object> env = new HashMap<>();

    private NodeExceptionSignal exceptionSignal;

    private boolean inLoop;

    /**
     * ReturnData node return data Map, take the last ReturnData node as the standard,
     * and return List<Map> when the outer Node is LoopByDataset/LoopByPage.
     */
    private Object returnData;

    public NodeContext(Map<String, Object> map) {
        this.env.putAll(map);
    }

    public void put(String key, Object value) {
        env.put(key, value);
    }

    public void putAll(Map<String, Object> map) {
        env.putAll(map);
    }

    public Object get(String key) {
        return env.get(key);
    }

    public boolean containsKey(String key) {
        return env.containsKey(key);
    }

    public boolean containsAll(Collection<String> keys) {
        return env.keySet().containsAll(keys);
    }

    public Set<String> keySet() {
        return env.keySet();
    }

    public void remove(String key) {
        env.remove(key);
    }

    public void clearExceptionSignal() {
        this.exceptionSignal = null;
    }

    /**
     * Copy the current environment variables for environment isolation in LoopByDataset/LoopByPage
     *
     * @return Copied environment variables
     */
    public NodeContext copy() {
        return new NodeContext(this.env);
    }
}
