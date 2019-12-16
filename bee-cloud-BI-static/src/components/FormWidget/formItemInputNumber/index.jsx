import React from 'react';
import PropTypes from 'prop-types';
import { Form, InputNumber } from 'antd';

import rulesConfig from './config';

const FormItem = Form.Item;

const FormItemInput = props => {
  const {
    getFieldDecorator,
    formItemLayout,
    label,
    extra,
    fieldId,
    initialValue,
    //---------

    //---------
    ruleType,
    validator,
    required,
    //---------

    inputProps,
  } = props;

  const selfValidator = (rule, value, callback) => {
    const pattern = /\s/g;
    if (pattern.test(value)) {
      callback('不能输入空格');
    }
    callback();
  };

  const customRules = rulesConfig[ruleType] || {};

  return (
    <FormItem {...formItemLayout} label={label} extra={extra}>
      {getFieldDecorator(fieldId, {
        initialValue,
        rules: [
          { ...customRules },
          {
            required,
            message: `${label}必填`,
          },
          { validator: validator || selfValidator },
        ],
      })(<InputNumber placeholder={`请输入${label}`} {...inputProps} />)}
    </FormItem>
  );
};

FormItemInput.propTypes = {
  getFieldDecorator: PropTypes.func.isRequired, // 表单控件
  fieldId: PropTypes.string.isRequired, // ID
  label: PropTypes.oneOfType([PropTypes.string, PropTypes.object]), // 名称
  formItemLayout: PropTypes.object, // 栅格
  initialValue: PropTypes.string, // 初始值

  ruleType: PropTypes.string, // 验证规则的类型
  validator: PropTypes.func, // 自定义验证函数

  inputProps: PropTypes.object, // 参照antd Input属性

  extra: PropTypes.oneOfType([PropTypes.string, PropTypes.element]),
  required: PropTypes.bool,
};

FormItemInput.defaultProps = {
  label: '表单输入框',
  formItemLayout: {
    labelCol: { span: 5 },
    wrapperCol: { span: 15 },
  },
  initialValue: undefined,

  ruleType: null,
  validator: undefined,

  inputProps: {},

  extra: '',
  required: true,
};

export default FormItemInput;
