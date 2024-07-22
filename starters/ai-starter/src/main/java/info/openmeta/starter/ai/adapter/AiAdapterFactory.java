package info.openmeta.starter.ai.adapter;

import info.openmeta.starter.ai.enums.AiModelProvider;
import info.openmeta.framework.base.utils.SpringContextUtils;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * AI Adapter Factory
 */
@Component
public class AiAdapterFactory {

    private final Map<AiModelProvider, AiAdapter> aiAdapterMap = new ConcurrentHashMap<>();

    /**
     * Get AI Adapter by model provider
     * @param aiProvider AI model provider
     * @return AI adapter
     */
    public AiAdapter getAiAdapter(AiModelProvider aiProvider) {
        AiAdapter aiAdapter = aiAdapterMap.get(aiProvider);
        if (aiAdapter != null) {
            return aiAdapter;
        }
        if (AiModelProvider.OPEN_AI.equals(aiProvider)) {
            aiAdapter = SpringContextUtils.getBeanByClass(OpenAIAdapter.class);
        } else {
            throw new IllegalArgumentException("AI model provider not yet supported:" + aiProvider);
        }
        aiAdapterMap.put(aiProvider, aiAdapter);
        return aiAdapter;
    }
}
