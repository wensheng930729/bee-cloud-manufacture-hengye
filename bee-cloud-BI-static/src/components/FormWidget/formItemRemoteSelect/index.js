import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { Form } from 'antd';

import RemoteSelect from './RemoteSelect';

const FormItem = Form.Item;

class FormItemRemoteSelect extends Component {
  static propTypes = {
    getFieldDecorator: PropTypes.func.isRequired, // 表单控件
    fieldId: PropTypes.string.isRequired, // ID
    label: PropTypes.oneOfType([PropTypes.string, PropTypes.object]), // 名称
    formItemLayout: PropTypes.object, // 栅格
    initialValue: PropTypes.oneOfType([PropTypes.string, PropTypes.array]), // 初始值
    extra: PropTypes.string, //
    hidden: PropTypes.bool, // 是否隐藏
    validator: PropTypes.func,
  };

  static defaultProps = {
    hidden: false,
    label: '基于表单的下拉框组件',
    formItemLayout: {
      labelCol: { span: 5 },
      wrapperCol: { span: 15 },
    },
    initialValue: [],
    extra: '',
    validator: undefined,
  };

  constructor(props) {
    super(props);
    this.state = {};
  }

  // --------验证方法
  validatorFunc = (rule, value, callback) => {
    callback();
  };

  render() {
    const {
      label,
      formItemLayout,
      getFieldDecorator,
      fieldId,
      initialValue,
      extra,
      hidden,
      required,
      validator,
      selectProps,
    } = this.props;

    return hidden ? null : (
      <FormItem {...formItemLayout} label={label} extra={extra}>
        {getFieldDecorator(fieldId, {
          initialValue,
          validateFirst: true,
          rules: [
            { validator: validator || this.validatorFunc },
            {
              required,
              message: `${label}必填`,
            },
          ],
        })(<RemoteSelect {...selectProps} />)}
      </FormItem>
    );
  }
}

export default FormItemRemoteSelect;
