package info.openmeta.starter.ai.service;

import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.starter.ai.entity.AiMessage;
import info.openmeta.starter.ai.entity.AiRobot;
import info.openmeta.starter.ai.vo.ChatMessage;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

/**
 * AiRobot Model Service Interface
 */
public interface AiRobotService extends EntityService<AiRobot, Long> {

    /**
     * Stream Chat
     *
     * @param chatMessage Chat message
     * @return SseEmitter
     */
    SseEmitter streamChat(ChatMessage chatMessage);

    /**
     * Chat API
     *
     * @param chatMessage Chat message
     * @return AiMessage
     */
    AiMessage chat(ChatMessage chatMessage);

}