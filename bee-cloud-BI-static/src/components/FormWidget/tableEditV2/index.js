import React, { PureComponent, Component } from 'react';
import { Table, Form } from 'antd';
import isEqual from 'lodash/isEqual';

import FormItemInput from '../formItemInput';
import FormItemSelect from '../formItemSelect';

import styles from './index.less';

/**
 *
 */
const EditableContext = React.createContext();

const EditableRow = ({ form, index, ...props }) => (
  <EditableContext.Provider value={form}>
    <tr {...props} />
  </EditableContext.Provider>
);

// 设置form的options
const formOptions = {
  name: 'EditableFormRow',
  onFieldsChange(props, changedFields) {
    if (props.onFieldsChange) {
      props.onFieldsChange(changedFields);
    }
  },
  mapPropsToFields(props) {
    const result = {};
    const { record, dataIndex } = props;
    if (record && dataIndex && record[dataIndex] !== undefined) {
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

  // 是否可编辑 的切换事件
  toggleEdit = () => {
    const { editing: oldediting } = this.state;
    const editing = !oldediting;
    this.setState({ editing }, () => {});
  };

  // 表单select改变事件
  handleSelectSave = (value, dataIndex) => {
    const { record, handleSave } = this.props;
    this.form.validateFields(error => {
      if (error) {
        return;
      }
      this.toggleEdit();
      const newRecord = { ...record };
      newRecord[dataIndex] = value;
      if (handleSave) {
        handleSave({ ...newRecord });
      }
    });
  };

  // 表单input 失去焦点事件
  handleInputSave = (e, dataIndex) => {
    const { record, handleSave } = this.props;
    const newRecord = { ...record };
    this.form.validateFields((error, values) => {
      if (error && error[e.currentTarget.id]) {
        return;
      }
      this.toggleEdit();
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
        return null;
    }
  };

  renderCell = form => {
    this.form = form;
    const { children, dataIndex, record, inputType, selectProps, inputProps, ...rest } = this.props;
    const { editing } = this.state;

    let selectdText = null;
    if (
      inputType === 'select' &&
      Array.isArray(selectProps.options) &&
      selectProps.options.length > 0 &&
      record[dataIndex]
    ) {
      const exitData = selectProps.options.filter(
        item => `${item.value}` === `${record[dataIndex]}`,
      );
      if (exitData.length > 0) selectdText = exitData[0].label;
    }
    return editing ? (
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
        selectProps,
        inputProps: {
          ...inputProps,
          onPressEnter: value => this.handleInputSave(value, dataIndex),
          onBlur: value => this.handleInputSave(value, dataIndex),
        },
      })
    ) : (
      <div
        className="editable-cell-value-wrap"
        style={{ paddingRight: 24 }}
        onClick={this.toggleEdit}
      >
        {selectdText || children}
      </div>
    );
  };

  render() {
    const { editable, ...restProps } = this.props;

    return (
      <td {...restProps}>
        {editable ? (
          <EditableContext.Consumer>{form => this.renderCell(form)}</EditableContext.Consumer>
        ) : (
          restProps.children
        )}
      </td>
    );
  }
}

/*

*/
class TableEdit extends PureComponent {
  static defaultProps = {
    columns: [],
    dataSource: [],
    handleTableSve: undefined,
  };

  static getDerivedStateFromProps(nextProps, curState) {
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

    this.state = {
      dataSource: props.dataSource,
      selfcolumns: props.columns,
    };
  }

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
    const { dataSource, selfcolumns } = this.state;
    const { rowKey, size, scroll, className, title, footer, ...rest } = this.props;

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
          editable: col.editable,
          dataIndex: col.dataIndex,
          title: col.title,
          inputType: col.inputType,
          inputProps: col.inputProps,
          selectProps: col.selectProps,
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
          rowSelection={undefined}
          dataSource={dataSource}
          columns={columns}
          size={size}
          scroll={scroll}
          pagination={false}
          footer={footer}
          // {...rest}
        />
      </div>
    );
  }
}

export default TableEdit;
