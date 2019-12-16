import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { Form, Select } from 'antd';

import isEqual from 'lodash/isEqual';

import { configOptions, configRule } from './configOptions';

const FormItem = Form.Item;
const { Option } = Select;

class FormItemSelect extends Component {
  static propTypes = {
    getFieldDecorator: PropTypes.func.isRequired, // 表单控件
    fieldId: PropTypes.string.isRequired, // ID
    label: PropTypes.oneOfType([PropTypes.string, PropTypes.object]), // 名称
    formItemLayout: PropTypes.object, // 栅格

    initialValue: PropTypes.oneOfType([PropTypes.string, PropTypes.number, PropTypes.array]), // 初始值

    extra: PropTypes.string, //

    hidden: PropTypes.bool, // 是否隐藏

    selectProps: PropTypes.shape({
      placeholder: PropTypes.string, // 占位符
      options: PropTypes.array, // value,label键值对
      // select组件的默认属性，详情请查看官方文档
    }),

    ruleType: PropTypes.string,

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
    selectProps: {
      placeholder: '请选择',
      // [ { value: '0', label: '全部' } ]
      options: [],
    },

    ruleType: null,

    validator: undefined,
  };

  static getDerivedStateFromProps(nextProps, curState) {
    // clean state
    if (
      Array.isArray(nextProps.selectProps.options) &&
      Array.isArray(curState.options) &&
      !isEqual(nextProps.selectProps.options, curState.options)
    ) {
      return {
        options: nextProps.selectProps.options,
      };
    }
    return null;
  }

  constructor(props) {
    super(props);
    this.state = {
      options: props.selectProps.options || [],
    };
  }

  onChange = value => {
    const { onChange } = this.props;
    if (onChange) {
      onChange(value);
    }
  };

  onSelect = value => {
    const { onSelectChange } = this.props;
    if (onSelectChange) {
      onSelectChange(value);
    }
  };

  // --------验证方法
  validatorFunc = (rule, value, callback) => {
    callback();
  };

  renderOptions = options => {
    let doms = [];
    if (Array.isArray(options) && options.length > 0) {
      doms = options.map(item => {
        const { disabled } = item;
        const value = String(item.value);
        return (
          <Option value={value} disabled={disabled} key={value}>
            {item.label}
          </Option>
        );
      });
    }
    return doms;
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

      selectProps,
      required,
      validator,
      className,
      ruleType,
    } = this.props;
    const { options } = this.state;

    const newRules = configRule[ruleType] || {};

    const arrayType =
      selectProps.mode === 'multiple'
        ? { required, message: `${label}必填`, type: 'array' }
        : { required, message: `${label}必填` };

    return hidden ? null : (
      <FormItem {...formItemLayout} label={label} extra={extra}>
        {getFieldDecorator(fieldId, {
          initialValue,
          validateFirst: true,
          rules: [arrayType, { validator: validator || this.validatorFunc }, { ...newRules }],
        })(
          <Select
            style={{ minWidth: 150, width: '100%' }}
            {...selectProps}
            showSearch={options.length > 5}
            optionFilterProp="children"
            filterOption={(input, option) =>
              option.props.children.toLowerCase().indexOf(input.toLowerCase()) >= 0
            }
            onChange={this.onChange}
            onSelect={this.onSelect}
          >
            {this.renderOptions(options)}
          </Select>,
        )}
      </FormItem>
    );
  }
}

export default FormItemSelect;
