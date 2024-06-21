package info.openmeta.framework.web.task;

import info.openmeta.framework.web.task.params.TaskHandlerParams;

public interface AsyncTaskHandler<T extends TaskHandlerParams>  {

    /**
     * Get the code of the asynchronous task handler.
     * @return The code of the asynchronous task handler.
     */
    String getAsyncTaskHandlerCode();

    /**
     * Get the parameter type required by the current handler.
     * @return The class type of the parameters.
     */
    Class<T> getParamsType();

    /**
     * Validate the data integrity of the asynchronous task parameters.
     * @param taskParams The parameters of the task to validate.
     */
    void validateParams(T taskParams);

    /**
     * Execute the asynchronous task.
     * @param taskParams The parameters for executing the task.
     */
    void execute(T taskParams);

}
