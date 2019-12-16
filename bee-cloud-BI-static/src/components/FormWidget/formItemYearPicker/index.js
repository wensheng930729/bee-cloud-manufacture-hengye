import React from 'react';
import PropTypes from 'prop-types';
import { Form } from 'antd';

import YearPicker from './YearPicker';

const FormItem = Form.Item;

const FormItemYearPicker = props => {
  const {
    getFieldDecorator,
    formItemLayout,
    fieldId,
    label,
    required,
    initialValue,
    // ----------------
    extra,
    datePickerProps,
    validator,
  } = props;

  const selfvalidator = (rule, value, callback) => {
    // console.log(rule, value, callback);
    callback();
  };

  return (
    <FormItem {...formItemLayout} label={label} extra={extra}>
      {getFieldDecorator(fieldId, {
        initialValue,
        rules: [{ required, message: '请选择日期!' }, { validator: validator || selfvalidator }],
      })(<YearPicker {...datePickerProps} />)}
    </FormItem>
  );
};

FormItemYearPicker.propTypes = {
  getFieldDecorator: PropTypes.func.isRequired, // 表单控件
  fieldId: PropTypes.string.isRequired, // ID
  label: PropTypes.oneOfType([PropTypes.string, PropTypes.object]), // 名称
  formItemLayout: PropTypes.object, // 栅格

  required: PropTypes.bool,
  initialValue: PropTypes.string,
  datePickerProps: PropTypes.object,
  validator: PropTypes.func,
  extra: PropTypes.oneOfType([PropTypes.string, PropTypes.element]),
};

FormItemYearPicker.defaultProps = {
  label: '基于表单的下拉框组件',
  formItemLayout: {
    labelCol: { span: 5 },
    wrapperCol: { span: 15 },
  },

  required: true,
  initialValue: '',
  datePickerProps: {},
  validator: undefined,
  extra: '',
};

export default FormItemYearPicker;
