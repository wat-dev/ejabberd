/* ejabberd database maintenance script from MySQL.
 * This file is a part of ejabberd-scripts repository,
 * https://github.com/dotdoom/ejabberd-scripts
 *
 * To perform an automated cleanup:
 * mysql> USE <your ejabberd database>
 * mysql> source ejabberd_maintenance.sql
 * mysql> CALL autoclean();
 *
 * This will copy the tables, optimize and clean them up,
 * then replace the original tables.
 * Yes the modifications during autoclean() are ignored.
 * You can see live status in the `maintenance_report` table.
 */

DELIMITER //

/*
 * Common maintenance procedures
 */
DROP PROCEDURE IF EXISTS maintenance_execute;
CREATE PROCEDURE maintenance_execute(query VARCHAR(255))
BEGIN
	SET @query = query;
	PREPARE statement FROM @query;
	EXECUTE statement;
	DEALLOCATE PREPARE statement;
END//

DROP PROCEDURE IF EXISTS maintenance_start_operation;
CREATE PROCEDURE maintenance_start_operation(operation VARCHAR(255))
BEGIN
	SELECT @operation_index := COALESCE(MAX(operation_index), -1)+1
		FROM maintenance_report
		WHERE report_id = @report_id;

	INSERT INTO maintenance_report (
		report_id,
		operation_name,
		operation_index,
		rows_affected,
		rows_to_affect,
		created_at,
		updated_at
	) VALUES (
		@report_id,
		operation,
		@operation_index,
		0,
		0,
		NOW(),
		NOW()
	);
END//

DROP PROCEDURE IF EXISTS maintenance_prepare;
CREATE PROCEDURE maintenance_prepare(operation VARCHAR(255))
BEGIN
	CREATE TABLE IF NOT EXISTS maintenance_report(
		report_id           INTEGER NOT NULL,
		operation_name      VARCHAR(255),
		operation_index     INTEGER NOT NULL,
		rows_affected       INTEGER,
		rows_to_affect      INTEGER,
		created_at          DATETIME,
		updated_at          DATETIME,
		PRIMARY KEY(report_id, operation_index)
	);

	SELECT @report_id := COALESCE(MAX(report_id), 0)+1 FROM maintenance_report;
	CALL maintenance_start_operation(operation);
END//

/*
 * Optimization procedures
 */
DROP PROCEDURE IF EXISTS optimization_drop_non_clustered_indexes;
CREATE PROCEDURE optimization_drop_non_clustered_indexes(target_table VARCHAR(255))
BEGIN
	DECLARE done INTEGER DEFAULT FALSE;
	DECLARE col_index_name VARCHAR(64);
	DECLARE col_non_unique BIGINT(1);
	DECLARE cur_indexes CURSOR FOR
		SELECT DISTINCT INDEX_NAME, NON_UNIQUE
			FROM information_schema.STATISTICS
			WHERE
				TABLE_SCHEMA = DATABASE() AND
				TABLE_NAME = target_table;
	DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;

	OPEN cur_indexes;
	read_loop:LOOP
		FETCH cur_indexes INTO col_index_name, col_non_unique;
		IF DONE THEN
			LEAVE read_loop;
		END IF;

		IF col_non_unique = 1 THEN /* This check for clusterity should be more smart */
			CALL maintenance_execute(CONCAT("ALTER TABLE `", target_table, "` DROP INDEX `", col_index_name, "`"));
		END IF;
	END LOOP;
	CLOSE cur_indexes;
END//

DROP PROCEDURE IF EXISTS optimization_copy_table;
CREATE PROCEDURE optimization_copy_table()
BEGIN
	CALL maintenance_start_operation(CONCAT("copying table ", @target_table));
	SET @target_table_copy = CONCAT(@target_table, "_for_optimization");
	CALL maintenance_execute(CONCAT("DROP TABLE IF EXISTS `", @target_table_copy, "`"));
	CALL maintenance_execute(CONCAT("CREATE TABLE `", @target_table_copy, "` LIKE `", @target_table, "`"));
	CALL optimization_drop_non_clustered_indexes(@target_table_copy);
	CALL maintenance_execute(CONCAT("ALTER TABLE `", @target_table_copy, "` ENGINE=InnoDB"));
	CALL maintenance_execute(CONCAT("INSERT INTO `", @target_table_copy, "` SELECT * FROM `", @target_table, "`"));
END//

DROP PROCEDURE IF EXISTS optimization_apply_table;
CREATE PROCEDURE optimization_apply_table()
BEGIN
	CALL maintenance_execute(CONCAT("RENAME TABLE `", @target_table, "` TO `", @target_table, "_for_removal`, `",
			@target_table, "_for_optimization` TO `", @target_table, "`"));
	CALL maintenance_execute(CONCAT("DROP TABLE `", @target_table, "_for_removal`"));
	UPDATE maintenance_report
		SET
			updated_at = NOW()
		WHERE
			report_id = @report_id AND
			operation_index = @operation_index;
END//

DROP PROCEDURE IF EXISTS optimization_copy;
CREATE PROCEDURE optimization_copy()
BEGIN
	CALL maintenance_prepare("starting optimization copy");

	SET @target_table = "users";
	CALL optimization_copy_table();

	SET @target_table = "last";
	CALL optimization_copy_table();

	SET @target_table = "privacy_default_list";
	CALL optimization_copy_table();

	SET @target_table = "rosterusers";
	CALL optimization_copy_table();
	CREATE INDEX i_rosteru_username ON rosterusers_for_optimization(username);
	CREATE INDEX i_rosteru_jid ON rosterusers_for_optimization(jid);

	SET @target_table = "rostergroups";
	CALL optimization_copy_table();
	CREATE INDEX pk_rosterg_user_jid ON rostergroups_for_optimization(username(75), jid(75));

	SET @target_table = "spool";
	CALL optimization_copy_table();
	CREATE INDEX i_despool ON spool_for_optimization(username);

	SET @target_table = "vcard";
	CALL optimization_copy_table();

	SET @target_table = "privacy_list";
	CALL optimization_copy_table();
	CREATE INDEX i_privacy_list_username ON privacy_list_for_optimization(username);

	SET @target_table = "privacy_list_data";
	CALL optimization_copy_table();
	CREATE INDEX i_privacy_l_d_id ON privacy_list_data_for_optimization(id);

	SET @target_table = "private_storage";
	CALL optimization_copy_table();
	CREATE INDEX i_private_storage_username USING BTREE ON private_storage(username);
END//

DROP PROCEDURE IF EXISTS optimization_apply;
CREATE PROCEDURE optimization_apply()
BEGIN
	CALL maintenance_prepare("starting optimization apply");

	SET @target_table = "users";
	CALL optimization_apply_table();

	SET @target_table = "last";
	CALL optimization_apply_table();

	SET @target_table = "privacy_default_list";
	CALL optimization_apply_table();

	SET @target_table = "rosterusers";
	CALL optimization_apply_table();

	SET @target_table = "rostergroups";
	CALL optimization_apply_table();

	SET @target_table = "spool";
	CALL optimization_apply_table();

	SET @target_table = "vcard";
	CALL optimization_apply_table();

	SET @target_table = "privacy_list";
	CALL optimization_apply_table();

	SET @target_table = "privacy_list_data";
	CALL optimization_apply_table();

	SET @target_table = "private_storage";
	CALL optimization_apply_table();
END//


/*
 * Cleanup procedures.
 *
 */
DROP PROCEDURE IF EXISTS autoclean_operation;
CREATE PROCEDURE autoclean_operation(operation VARCHAR(255), target_table VARCHAR(255), conditions VARCHAR(255))
BEGIN
	SET
		@rows_to_affect = 0,
		@rows_affected = 0;

	CALL maintenance_start_operation(operation);

	CALL maintenance_execute(CONCAT("SELECT @rows_to_affect := COUNT(*) FROM ", target_table, "_for_optimization WHERE ", conditions));

	UPDATE maintenance_report
		SET
			rows_to_affect = @rows_to_affect,
			updated_at = NOW()
		WHERE
			report_id = @report_id AND
			operation_index = @operation_index;

	SET @query = CONCAT("DELETE FROM ", target_table, "_for_optimization WHERE ", conditions, " LIMIT 2000");
	PREPARE operation_proceed FROM @query;
	operation:LOOP
		EXECUTE operation_proceed;
		SET @affected_now = ROW_COUNT();
		IF @affected_now <= 0 THEN
			LEAVE operation;
		END IF;
		SET @rows_affected = @rows_affected + @affected_now;
		UPDATE maintenance_report
			SET
				rows_affected = @rows_affected,
				updated_at = NOW()
			WHERE
				report_id = @report_id AND
				operation_index = @operation_index;
	END LOOP operation;
	DEALLOCATE PREPARE operation_proceed;
END//

DROP PROCEDURE IF EXISTS autoclean_users_dependencies;
CREATE PROCEDURE autoclean_users_dependencies(target_table VARCHAR(255))
BEGIN
	CALL autoclean_operation(CONCAT("removing dangling users from '", target_table, "'"), target_table, CONCAT("
		NOT EXISTS(SELECT * FROM users_for_optimization users WHERE users.username = ", target_table, "_for_optimization.username)"));
END//

DROP PROCEDURE IF EXISTS autoclean_prepare;
CREATE PROCEDURE autoclean_prepare(cleanup_type VARCHAR(255), months INTEGER)
BEGIN
	CALL maintenance_prepare(CONCAT("starting cleanup: ", cleanup_type));
	SET @barrier = DATE_ADD(NOW(), INTERVAL -months MONTH);
END//

DROP PROCEDURE IF EXISTS autoclean_users;
CREATE PROCEDURE autoclean_users(months INTEGER)
BEGIN
	CALL autoclean_prepare("users with dependencies", months);

	CALL autoclean_operation("removing outdated / unused accounts", "users", "
		username IN (
			SELECT username
				FROM last_for_optimization
				WHERE
					seconds IS NULL OR
					FROM_UNIXTIME(seconds) < @barrier)");

	CALL autoclean_operation("removing never used accounts", "users", "
		NOT EXISTS (
			SELECT 1
				FROM last_for_optimization
				WHERE
					last_for_optimization.username = users_for_optimization.username)");

	CALL autoclean_users_dependencies("last");
	CALL autoclean_users_dependencies("spool");
	CALL autoclean_users_dependencies("privacy_default_list");
	CALL autoclean_users_dependencies("rosterusers");
	CALL autoclean_users_dependencies("rostergroups");
	CALL autoclean_users_dependencies("vcard");
	CALL autoclean_users_dependencies("privacy_list");
	CALL autoclean_users_dependencies("private_storage");

	CALL autoclean_operation("removing dangling privacy list data", "privacy_list_data",
		"id NOT IN (SELECT id FROM privacy_list_for_optimization)");
END//

DROP PROCEDURE IF EXISTS autoclean_spool;
CREATE PROCEDURE autoclean_spool(months INTEGER)
BEGIN
	CALL autoclean_prepare("spool data", months);

	CALL autoclean_operation("removing old spool items", "spool", "
		created_at < @barrier");
END//

DROP PROCEDURE IF EXISTS autoclean;
CREATE PROCEDURE autoclean()
BEGIN
	CALL optimization_copy();
	CALL autoclean_users(6);
	CALL autoclean_spool(3);
	CALL optimization_apply();
END//

DELIMITER ;
