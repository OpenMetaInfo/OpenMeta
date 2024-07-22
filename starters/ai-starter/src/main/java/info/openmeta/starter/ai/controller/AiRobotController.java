package info.openmeta.starter.ai.controller;

import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.ai.entity.AiMessage;
import info.openmeta.starter.ai.entity.AiRobot;
import info.openmeta.starter.ai.service.AiRobotService;
import info.openmeta.starter.ai.vo.ChatMessage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

/**
 * AiRobot Model Controller
 */
@Tag(name = "AiRobot")
@RestController
@RequestMapping("/AiRobot")
public class AiRobotController extends EntityController<AiRobotService, AiRobot, Long> {

    @Operation(summary = "Stream Chat", description = "Stream chat by SSE(Server-Sent Events).")
    @PostMapping("/streamChat")
    public SseEmitter streamChat(@RequestBody @Valid ChatMessage chatMessage) {
        return service.streamChat(chatMessage);
    }

    @Operation(summary = "Chat API", description = "Non-streaming Chat API")
    @PostMapping(value = "/chat")
    public ApiResponse<AiMessage> chat(@RequestBody @Valid ChatMessage chatMessage) {
        AiMessage aiMessage = service.chat(chatMessage);
        return ApiResponse.success(aiMessage);
    }

}