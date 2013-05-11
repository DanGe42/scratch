import gdata.spreadsheet.service

class SheetHelper(object):
    def __init__(self, email, password, spreadsheet_key):
        client = gdata.spreadsheet.service.SpreadsheetsService()
        client.ClientLogin(email, password)

        rows = client.GetListFeed(spreadsheet_key).entry

        # pick a row (they all contain header data)
        header_row = rows[0]
        task_col, col_keys = self._process_header(header_row)

        task_names, completed_tasks = self._process_data(rows, col_keys, task_col)
        # WARN: `data` has been mutated!

        self.scores = completed_tasks
        self.tasks = task_names


    def _process_header(self, header):
        # task_col is the task column. This represents the tasks that residents
        # must complete
        task_col = None

        # col_keys is a dictionary of floors. Each floor points to the list of
        # tasks completed
        col_keys = []

        for key in header.custom:
            if key[0] == '_':
                task_col = key
            else:
                # TODO: input validation
                col_keys.append(key)

        if task_col is None:
            raise ValueError("No first column found")
        return task_col, col_keys


    def _process_data(self, data, col_keys, task_col):
        completed_tasks = {}
        task_names = []
        for key in col_keys:
            completed_tasks[key] = []

        for row in data:
            task_name = row.custom[task_col].text
            task_names.append(task_name)
            del row.custom[task_col]

            # NOTE: row.custom has been modified!
            for key in row.custom:
                value = row.custom[key].text

                # We assume that if the cell has something, then it must mean
                # that the task has been completed
                # In addition, we want to ensure that no hidden entries (only
                # whitespace) are present (arbitrary decision)
                if value is not None and len(value.strip()) > 0:
                    print value
                    completed_tasks[key].append(task_name)

        return task_names, completed_tasks

    @property
    def teams(self):
        return self.scores.keys()

    def tasks_completed(self, team):
        return len(self.scores[team])
