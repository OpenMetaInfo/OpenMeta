package info.openmeta.starter.flow.action;

import info.openmeta.starter.flow.enums.ActionExceptionSignal;
import lombok.Data;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Action context environment variables, including input data,
 * and the {actionCode: returnValue} pair after each FlowAction execution
 */
@Data
public class ActionContext {

    private final Map<String, Object> env = new HashMap<>();

    private ActionExceptionSignal exceptionSignal;

    private boolean inLoop;

    /**
     * ReturnData action return data Map, take the last ReturnData action as the standard,
     * and return List<Map> when the outer Node is LoopByDataset/LoopByPage.
     */
    private Object returnData;

    public ActionContext(Map<String, Object> map) {
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
    public ActionContext copy() {
        return new ActionContext(this.env);
    }
}
