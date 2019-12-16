import React from 'react';
import PropTypes from 'prop-types';
import { Form } from 'antd';
import moment from 'moment';
import CustomRangePicker from './CustomRangePicker';

const FormItem = Form.Item;

const FormItemRangePicker = props => {
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
    callback();
  };

  return (
    <FormItem {...formItemLayout} label={label} extra={extra}>
      {getFieldDecorator(fieldId, {
        initialValue,
        rules: [{ required, message: '请选择日期!' }, { validator: validator || selfvalidator }],
      })(<CustomRangePicker {...datePickerProps} />)}
    </FormItem>
  );
};

FormItemRangePicker.propTypes = {
  getFieldDecorator: PropTypes.func.isRequired, // 表单控件
  fieldId: PropTypes.string.isRequired, // ID
  label: PropTypes.oneOfType([PropTypes.string, PropTypes.object]), // 名称
  formItemLayout: PropTypes.object, // 栅格

  required: PropTypes.bool,
  initialValue: PropTypes.object,
  datePickerProps: PropTypes.object,
  validator: PropTypes.func,
  extra: PropTypes.oneOfType([PropTypes.string, PropTypes.element]),
};

FormItemRangePicker.defaultProps = {
  label: '基于表单的下拉框组件',
  formItemLayout: {
    labelCol: { span: 5 },
    wrapperCol: { span: 15 },
  },

  required: false,
  initialValue: {
    startTime: moment()
      .subtract(30, 'days')
      .format('YYYY-MM-DD'),
    endTime: moment().format('YYYY-MM-DD'),
  },
  datePickerProps: {},
  validator: undefined,
  extra: '',
};

export default FormItemRangePicker;
