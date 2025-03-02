package info.openmeta.starter.flow.event;

import info.openmeta.framework.orm.changelog.ChangeLogHolder;
import info.openmeta.framework.orm.changelog.event.TransactionEvent;
import info.openmeta.framework.orm.changelog.message.dto.ChangeLog;
import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.starter.flow.FlowAutomation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

import java.util.List;

/**
 * Transaction listener for transactional Flow.
 * Before the transaction is committed, trigger the Flow process to check the data.
 */
@Component
public class TransactionListenerForFlow {

    @Autowired
    private FlowAutomation flowAutomation;

    /**
     * Before the transaction is committed, verify the data change with ChangeLog.
     *
     * @param event Transaction event
     */
    @TransactionalEventListener(phase = TransactionPhase.BEFORE_COMMIT)
    public void beforeCommit(TransactionEvent event) {
        Context context = ContextHolder.getContext();
        if (context.isTriggerFlow()) {
            // Triggers data verification synchronously. The verification process cannot submit any data changes again.
            List<ChangeLog> changeLogs = ChangeLogHolder.get();
            flowAutomation.triggerSyncFlowByChangeLog(changeLogs);
        }
    }
}
