import React, { Component, PureComponent } from 'react';
import { Table, Form, Select } from 'antd';
import isEqual from 'lodash/isEqual';

import FormItemInput from '../formItemInput';
import FormItemSelect from '../formItemSelect';

// import { EditableFormRow, EditableCell } from './editCompoent';

import styles from './index.less';

const EditableContext = React.createContext();

const { Option } = Select;

const EditableRow = ({ form, index, ...props }) => (
  <EditableContext.Provider value={form}>
    <tr {...props} />
  </EditableContext.Provider>
);

const formOptions = {
  name: 'global_state',
  onFieldsChange(props, changedFields) {
    if (props.onFieldsChange) {
      props.onFieldsChange(changedFields);
    }
  },
  mapPropsToFields(props) {
    const result = {};
    const { record, dataIndex } = props;
    if (record && dataIndex) {
      const obj = {};
      obj[dataIndex] = record[dataIndex];
      Object.keys(obj).forEach(key => {
        result[key] = Form.createFormField({
          value: obj[key],
        });
      });
    }

    return result;
  },
  // onValuesChange(_, values) {
  //   console.log(values);
  // },
};

const EditableFormRow = Form.create(formOptions)(EditableRow);

class EditableCell extends Component {
  state = {
    editing: false,
  };

  toggleEdit = () => {
    const { editing: oldediting } = this.state;
    const editing = !oldediting;
    this.setState({ editing }, () => {
      if (editing) {
        this.input.focus();
      }
    });
  };

  handleSelectSave = (value, dataIndex) => {
    const { record, handleSave } = this.props;
    this.form.validateFields(error => {
      if (error) {
        return;
      }
      // this.toggleEdit();
      const newRecord = { ...record };
      newRecord[dataIndex] = value;
      if (handleSave) {
        handleSave({ ...newRecord });
      }
    });
  };

  handleBlurSave = (e, dataIndex) => {
    const { record, handleSave } = this.props;
    const newRecord = { ...record };
    this.form.validateFields((error, values) => {
      if (error && error[e.currentTarget.id]) {
        return;
      }
      // this.toggleEdit();
      newRecord[dataIndex] = e.target.value;
      handleSave({ ...newRecord });
    });
  };

  getInputComponent = formItemProps => {
    const { inputType } = formItemProps;

    switch (inputType) {
      case 'input':
        return <FormItemInput {...formItemProps} />;

      case 'select':
        return <FormItemSelect {...formItemProps} />;

      default:
        break;
    }
  };

  renderSelect = datas => {
    let doms = [];
    if (Array.isArray(datas) && datas.length > 0) {
      doms = datas.map(item => (
        <Option value={item.value} key={item.value}>
          {item.label}
        </Option>
      ));
    }
    return doms;
  };

  render() {
    const { editing } = this.state;
    const {
      editable,
      dataIndex,
      title,
      record,
      index,
      handleSave,
      inputType,
      disabled,
      selectOptions,
      ...restProps
    } = this.props;

    const newditing = true;

    return (
      <td {...restProps}>
        {editable ? (
          <EditableContext.Consumer>
            {form => {
              this.form = form;

              return newditing ? (
                this.getInputComponent({
                  form,
                  getFieldDecorator: form.getFieldDecorator,
                  label: '',
                  fieldId: dataIndex,
                  initialValue: ['select', 'input'].includes(inputType)
                    ? `${record[dataIndex]}`
                    : record[dataIndex],
                  formItemLayout: {},
                  inputType,
                  required: false,
                  onSelectChange: value => this.handleSelectSave(value, dataIndex),
                  selectProps: {
                    ref: node => {
                      this.select = node;
                    },
                    disabled,
                    options: selectOptions,
                  },
                  inputProps: {
                    ref: node => {
                      this.input = node;
                    },
                    onPressEnter: value => this.handleBlurSave(value, dataIndex),
                    onBlur: value => this.handleBlurSave(value, dataIndex),
                    disabled,
                  },
                })
              ) : (
                <div
                  className={styles.editableCellValueWrap}
                  style={{ paddingRight: 24 }}
                  onClick={this.toggleEdit}
                >
                  {restProps.children}
                </div>
              );
            }}
          </EditableContext.Consumer>
        ) : (
          restProps.children
        )}
      </td>
    );
  }
}

// eslint-disable-next-line react/no-multi-comp
class TableEdit extends PureComponent {
  static defaultProps = {
    columns: [],
    selectedRowKeys: [],
    onChange: undefined,
    dataSource: [],
    hasRowSelection: false,
    handleTableSve: undefined,
  };

  static getDerivedStateFromProps(nextProps, curState) {
    // clean state
    const bool1 =
      Array.isArray(nextProps.columns) && !isEqual(nextProps.columns, curState.selfcolumns);
    const bool2 =
      Array.isArray(nextProps.dataSource) && !isEqual(nextProps.dataSource, curState.dataSource);

    if (bool1 || bool2) {
      return {
        dataSource: nextProps.dataSource,
        selfcolumns: nextProps.columns,
      };
    }
    return null;
  }

  constructor(props) {
    super(props);
    const { dataSource, selectedRowKeys } = props;
    this.state = {
      dataSource,
      selectedRowKeys,
      selfcolumns: [],
    };
  }

  // 复选框事件
  handleRowSelectChange = (selectedRowKeys, selectedRows) => {
    const { onSelectRow } = this.props;
    if (onSelectRow) {
      onSelectRow(selectedRows);
    }

    this.setState({ selectedRowKeys });
  };

  // 分页事件
  handleTableChange = (pagination, filters, sorter) => {
    const { onChange } = this.props;
    if (onChange) {
      onChange(pagination, filters, sorter);
    }
  };

  // 编辑表格改变事件
  handleSave = row => {
    const { dataSource } = this.state;
    const { handleTableSve } = this.props;
    const newData = [...dataSource];
    const index = newData.findIndex(item => row.key === item.key);
    const item = newData[index];
    newData.splice(index, 1, {
      ...item,
      ...row,
    });

    if (handleTableSve) {
      handleTableSve(newData);
    }

    this.setState({ dataSource: newData });
  };

  render() {
    const { selectedRowKeys, dataSource, selfcolumns } = this.state;
    const { rowKey, hasRowSelection, size, scroll, className, title, footer, ...rest } = this.props;

    const rowSelection = {
      selectedRowKeys,
      onChange: this.handleRowSelectChange,
    };

    const components = {
      body: {
        row: EditableFormRow,
        cell: EditableCell,
      },
    };
    const columns = selfcolumns.map(col => {
      if (!col.editable) {
        return col;
      }

      return {
        ...col,
        onCell: record => ({
          record,
          editable: record.editable,
          dataIndex: col.dataIndex,
          title: col.title,
          inputType: col.inputType,
          disabled: record.disabled,
          selectOptions: col.selectOptions,
          handleSave: this.handleSave,
        }),
      };
    });

    return (
      <div className={styles.standardTable}>
        <Table
          title={title}
          className={className}
          components={components}
          rowClassName={() => styles.editableRow}
          rowKey={rowKey || 'key'}
          rowSelection={hasRowSelection ? rowSelection : undefined}
          dataSource={dataSource}
          columns={columns}
          size={size}
          scroll={scroll}
          pagination={false}
          onChange={this.handleTableChange}
          footer={footer}
          // {...rest}
        />
      </div>
    );
  }
}

export default TableEdit;
