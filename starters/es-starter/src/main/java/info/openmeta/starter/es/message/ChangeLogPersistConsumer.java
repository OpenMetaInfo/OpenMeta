package info.openmeta.starter.es.message;

import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.orm.changelog.message.dto.ChangeLog;
import info.openmeta.framework.orm.changelog.message.dto.ChangeLogMessage;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import org.springframework.data.elasticsearch.core.mapping.IndexCoordinates;
import org.springframework.data.elasticsearch.core.query.IndexQuery;
import org.springframework.data.elasticsearch.core.query.IndexQueryBuilder;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * ChangeLog persist consumer
 */
@Slf4j
@Component
@RocketMQMessageListener(topic = "${rocketmq.topics.change-log.topic}", consumerGroup = "${rocketmq.topics.change-log.persist-group}")
public class ChangeLogPersistConsumer implements RocketMQListener<ChangeLogMessage> {

    @Value("${spring.elasticsearch.index.changelog}")
    private String index;

    @Autowired
    private ElasticsearchOperations esOperations;

    @Override
    public void onMessage(ChangeLogMessage changeLogMessage) {
        persistChangeLogToESDirectly(changeLogMessage);
    }

    /**
     * Persist change log to ES directly
     * @param changeLogMessage ChangeLog message
     */
    public void persistChangeLogToESDirectly(ChangeLogMessage changeLogMessage) {
        ContextHolder.setContext(changeLogMessage.getContext());
        // Build document list
        List<IndexQuery> indexQueries = new ArrayList<>();
        for (ChangeLog changeLog : changeLogMessage.getChangeLogs()) {
            IndexQuery indexQuery = new IndexQueryBuilder()
                    .withIndex(index)
                    .withId(UUID.randomUUID().toString())
                    .withObject(changeLog)
                    .build();
            indexQueries.add(indexQuery);
        }
        try {
            esOperations.bulkIndex(indexQueries, IndexCoordinates.of(index));
        } catch (Exception e) {
            throw new IllegalArgumentException(e.getMessage(), e);
        }
    }

}
