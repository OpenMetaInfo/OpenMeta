package info.openmeta.framework.web.task;

import info.openmeta.framework.base.exception.JSONException;
import info.openmeta.framework.web.task.params.TaskHandlerParams;
import info.openmeta.framework.orm.utils.BeanTool;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Asynchronous Task Factory
 */
@Component
public class AsyncTaskFactory<T extends TaskHandlerParams> {

    /**
     * Asynchronous task handler mapping: handler code to handler implementation.
     */
    private final Map<String, AsyncTaskHandler<T>> taskHandlerMap = new HashMap<>();

    /**
     * Constructor injection of all AsyncTaskHandler implementations.
     *
     * @param asyncTaskHandlers Spring auto-wired collection, automatically collects
     *                          all implementations of AsyncTaskHandler.
     */
    @Autowired
    public AsyncTaskFactory(List<AsyncTaskHandler<T>> asyncTaskHandlers) {
        for (AsyncTaskHandler<T> asyncTaskHandler : asyncTaskHandlers) {
            taskHandlerMap.put(asyncTaskHandler.getAsyncTaskHandlerCode(), asyncTaskHandler);
        }
    }

    /**
     * Execute asynchronous task.
     * @param asyncTaskHandlerCode The code of the asynchronous task handler.
     * @param asyncTaskParams The parameters of the asynchronous task.
     */
    public void executeAsyncTask(String asyncTaskHandlerCode, Map<String, Object> asyncTaskParams) {
        if (taskHandlerMap.containsKey(asyncTaskHandlerCode)) {
            AsyncTaskHandler<T> asyncTaskHandler = taskHandlerMap.get(asyncTaskHandlerCode);
            Class<T> paramsClass = asyncTaskHandler.getParamsType();
            T taskHandlerParams;
            try {
                taskHandlerParams = BeanTool.originalMapToObject(asyncTaskParams, paramsClass);
            } catch (JSONException e) {
                throw new JSONException("Failed to convert asynchronous task {0} parameters to {1} object: {2}",
                        asyncTaskHandlerCode, paramsClass.getSimpleName(), e.getMessage());
            }
            asyncTaskHandler.validateParams(taskHandlerParams);
            asyncTaskHandler.execute(taskHandlerParams);
        }
    }

}
