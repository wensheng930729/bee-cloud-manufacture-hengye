import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { Form, Input } from 'antd';

import rulesConfig from './config';

const FormItem = Form.Item;
const { TextArea } = Input;

class FormItemTextArea extends Component {
  static propTypes = {
    getFieldDecorator: PropTypes.func.isRequired, // 表单控件
    fieldId: PropTypes.string.isRequired, // ID
    label: PropTypes.oneOfType([PropTypes.string, PropTypes.object]), // 名称
    formItemLayout: PropTypes.object, // 栅格
    initialValue: PropTypes.string, // 初始值

    hidden: PropTypes.bool, // 是否隐藏

    ruleType: PropTypes.string, // 验证规则的类型
    validator: PropTypes.func, // 自定义验证函数

    textAreaProps: PropTypes.object, // 参照antd TextArea属性
  };

  static defaultProps = {
    hidden: false,
    label: '表单输入框',
    formItemLayout: {
      labelCol: { span: 5 },
      wrapperCol: { span: 15 },
    },
    initialValue: null,

    ruleType: null,
    validator: undefined,

    textAreaProps: {},
  };

  constructor(props) {
    super(props);
    this.state = {};
  }

  validator = (rule, value, callback) => {
    const pattern = /\s/g;
    if (pattern.test(value)) {
      callback('不能输入空格');
    }
    callback();
  };

  render() {
    const {
      getFieldDecorator,
      formItemLayout,
      label,
      extra,
      fieldId,
      initialValue,
      //---------
      hidden,
      //---------
      ruleType,
      validator,
      required,
      //---------

      textAreaProps,
    } = this.props;

    const customRules = rulesConfig[ruleType] || {};

    return hidden ? null : (
      <FormItem {...formItemLayout} label={label} extra={extra}>
        {getFieldDecorator(fieldId, {
          initialValue,
          rules: [
            {
              required,
              message: `${label}必填`,
            },
            { ...customRules },
            { validator: validator || this.validator },
          ],
        })(
          <TextArea
            placeholder={`请输入${label}`}
            autosize={{ minRows: 4, maxRows: 10 }}
            {...textAreaProps}
          />,
        )}
      </FormItem>
    );
  }
}

export default FormItemTextArea;
