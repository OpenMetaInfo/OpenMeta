package info.openmeta.framework.orm.changelog.event;

import info.openmeta.framework.orm.changelog.ChangeLogHolder;
import info.openmeta.framework.orm.changelog.message.ChangeLogProducer;
import info.openmeta.framework.orm.changelog.message.dto.ChangeLog;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

import java.util.List;

/**
 * Transaction listener, send ChangeLog after transaction commit.
 */
@Component
public class TransactionEventListener {

    @Autowired
    private ChangeLogProducer changeLogProducer;

    /**
     * Send ChangeLog after transaction commit, and clear ChangeLog in threadLocal after sending.
     * @param event Transaction event
     */
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    public void afterCommit(TransactionEvent event) {
        List<ChangeLog> changeLogs = ChangeLogHolder.get();
        changeLogProducer.sendChangeLog(changeLogs);
        ChangeLogHolder.clear();
    }

    /**
     * Clear ChangeLog in threadLocal after transaction rollback.
     * @param event Transaction event
     */
    @TransactionalEventListener(phase = TransactionPhase.AFTER_ROLLBACK)
    public void onRollback(TransactionEvent event) {
        ChangeLogHolder.clear();
    }
}
