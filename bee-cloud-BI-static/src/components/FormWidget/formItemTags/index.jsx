import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { Form } from 'antd';

import EditableTagGroup from './EditableTagGroup';

const FormItem = Form.Item;

class FormItemTags extends Component {
  static propTypes = {
    getFieldDecorator: PropTypes.func.isRequired, // 表单控件
    fieldId: PropTypes.string.isRequired, // ID
    label: PropTypes.oneOfType([PropTypes.string, PropTypes.object]), // 名称
    formItemLayout: PropTypes.object, // 栅格
    initialValue: PropTypes.string, // 初始值

    hidden: PropTypes.bool, // 是否隐藏

    ruleType: PropTypes.string, // 验证规则的类型
    validator: PropTypes.func, // 自定义验证函数
  };

  static defaultProps = {
    hidden: false,
    label: '表单标签',
    formItemLayout: {
      labelCol: { span: 5 },
      wrapperCol: { span: 15 },
    },
    initialValue: null,

    ruleType: null,
    validator: undefined,
  };

  constructor(props) {
    super(props);
    this.state = {};
  }

  validator = (rule, value, callback) => {
    const pattern = /\s/g;

    let bool = false;

    if (Array.isArray(value)) {
      const datas = value.filter(item => {
        const bool1 = typeof item === 'string' && pattern.test(item);
        const bool2 = item.name && pattern.test(item.name);
        return bool1 || bool2;
      });
      bool = datas.length > 0;
    }
    if (bool) {
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

      validator,
      required,
      //---------
    } = this.props;

    return hidden ? null : (
      <FormItem {...formItemLayout} label={label} extra={extra}>
        {getFieldDecorator(fieldId, {
          initialValue,
          rules: [
            {
              required,
              message: `${label}必填`,
            },
            { validator: validator || this.validator },
          ],
        })(<EditableTagGroup />)}
      </FormItem>
    );
  }
}

export default FormItemTags;
