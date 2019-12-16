import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { Form, TreeSelect } from 'antd';

const { SHOW_PARENT } = TreeSelect;
const FormItem = Form.Item;

class FormItemTreeSelect extends Component {
  static propTypes = {
    getFieldDecorator: PropTypes.func.isRequired, // 表单控件
    fieldId: PropTypes.string.isRequired, // ID
    label: PropTypes.oneOfType([PropTypes.string, PropTypes.object]), // 名称
    formItemLayout: PropTypes.object, // 栅格

    initialValue: PropTypes.oneOfType([PropTypes.string, PropTypes.number, PropTypes.array]), // 初始值

    extra: PropTypes.string, //

    hidden: PropTypes.bool, // 是否隐藏

    treeSelectProps: PropTypes.shape({
      placeholder: PropTypes.string, // 占位符
      options: PropTypes.array, // value,label键值对
      // select组件的默认属性，详情请查看官方文档
    }),

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
    treeSelectProps: {
      placeholder: '请选择',
      // [ { value: '0', label: '全部' } ]
      treeData: [],
    },

    validator: undefined,
  };

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

      treeSelectProps,
      required,
      validator,
      className,
    } = this.props;

    const tProps = {
      // treeData,
      // value: this.state.value,
      // onChange: this.onChange,
      // treeCheckable: true,
      showCheckedStrategy: SHOW_PARENT,
      searchPlaceholder: '请选择',
      style: {
        width: '100%',
      },
      ...treeSelectProps,
    };

    return hidden ? null : (
      <FormItem {...formItemLayout} label={label} extra={extra}>
        {getFieldDecorator(fieldId, {
          initialValue,
          validateFirst: true,
          rules: [
            { required, message: `${label}必填` },
            { validator: validator || this.validatorFunc },
          ],
        })(<TreeSelect {...tProps} />)}
      </FormItem>
    );
  }
}

export default FormItemTreeSelect;
