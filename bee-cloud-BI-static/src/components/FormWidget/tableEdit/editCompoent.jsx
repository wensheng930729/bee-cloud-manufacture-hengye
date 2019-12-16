import React, { Component } from 'react';
import { Form, Select } from 'antd';
import { FormItemInput, FormItemSelect } from 'component/FormWidgets';

import styles from './index.less';

const EditableContext = React.createContext();

const { Option } = Select;

const EditableRow = ({ form, index, ...props }) => (
  <EditableContext.Provider value={form}>
    <tr {...props} />
  </EditableContext.Provider>
);

const EditableFormRow = Form.create()(EditableRow);

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

  handleSelectSave = (value, option, dataIndex) => {
    const { record, handleTableSave } = this.props;
    this.form.validateFields(error => {
      if (error) {
        return;
      }
      this.toggleEdit();
      const newRecord = { ...record };
      newRecord[dataIndex] = value;
      if (handleTableSave) {
        handleTableSave({ ...newRecord });
      }
    });
  };

  handleBlurSave = e => {
    const { record, handleSave } = this.props;
    this.form.validateFields((error, values) => {
      if (error && error[e.currentTarget.id]) {
        return;
      }
      this.toggleEdit();
      handleSave({ ...record, ...values });
    });
  };

  getInputComponent = formItemProps => {
    const { inputType } = this.props;

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
      selectOptions,
      editable,
      dataIndex,
      title,
      record,
      index,
      handleSave,
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
                  getFieldDecorator: form.getFieldDecorator,
                  label: '',
                  fieldId: dataIndex,
                  initialValue: record[dataIndex],
                  formItemLayout: {},
                  ref: node => {
                    this.input = node;
                  },
                  onSelect: (value, option) => this.handleSelectSave(value, option, dataIndex),
                  onBlur: this.handleBlurSave,
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

export { EditableFormRow, EditableCell };
