package info.openmeta.starter.flow.controller;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.annotation.DataMask;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.flow.FlowAutomation;
import info.openmeta.starter.flow.message.dto.FlowEventMessage;
import info.openmeta.starter.flow.vo.TriggerEventVO;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Arrays;
import java.util.Map;

/**
 * Flow automation controller
 */
@Tag(name = "Flow Automation")
@RestController
@RequestMapping("/automation")
public class AutomationController {

    @Autowired
    private Environment env;

    @Autowired
    private FlowAutomation automation;

    @Operation(summary = "Button Event", description = "Trigger the flow by button event. ")
    @PostMapping("/buttonEvent")
    public ApiResponse<Object> buttonEvent(@RequestBody @Valid TriggerEventVO triggerEventVO) {
        return ApiResponse.success(automation.buttonEvent(triggerEventVO));
    }

    @Operation(summary = "API Event", description = "Trigger the flow by API event. ")
    @PostMapping("/apiEvent")
    @DataMask
    public ApiResponse<Object> apiEvent(@RequestBody @Valid TriggerEventVO triggerEventVO) {
        return ApiResponse.success(automation.apiEvent(triggerEventVO));
    }

    /**
     * Handles onchange events that cause changes in other field values.
     *
     * @param triggerEventVO The event parameters that include current data and field changes.
     * @return An ApiResponse containing a Map of other field values affected by the change.
     */
    @Operation(summary = "Onchange Event",
            description = "Pass the current data and return a Map of field value changes that affect other fields.")
    @PostMapping("/onchange")
    @DataMask
    public ApiResponse<Map<String, Object>> onchange(@RequestBody @Valid TriggerEventVO triggerEventVO) {
        return ApiResponse.success(automation.onchangeEvent(triggerEventVO));
    }

    /**
     * Simulates an event message for flow triggering.
     * This method is used for testing in non-production environments, such as dev, test and uat.
     *
     * @param flowEventMessage The event message used to simulate the flow trigger.
     * @return An ApiResponse containing the result of the simulated flow trigger.
     */
    @Operation(summary = "Simulate Event Message", description = """
            Simulate flow triggering by passing a FlowEventMessage, suitable for scenarios such as ChangeLog, Cron, etc.
            For non-production environment testing only.""")
    @PostMapping("/simulateEvent")
    public ApiResponse<Object> buttonEvent(@RequestBody FlowEventMessage flowEventMessage) {
        String[] profiles = env.getActiveProfiles();
        Assert.notTrue(Arrays.asList(profiles).contains("prod"),
                "This API is only open to non-production environments!");
        return ApiResponse.success(automation.triggerFlow(flowEventMessage, true));
    }
}
