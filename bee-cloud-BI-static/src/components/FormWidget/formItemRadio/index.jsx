import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { Form, Radio } from 'antd';

const FormItem = Form.Item;

class FormItemRadio extends Component {
  static propTypes = {
    getFieldDecorator: PropTypes.func.isRequired, // 表单控件
    fieldId: PropTypes.string.isRequired, // ID
    label: PropTypes.oneOfType([PropTypes.string, PropTypes.object]), // 名称
    formItemLayout: PropTypes.object, // 栅格
    initialValue: PropTypes.string, // 初始值

    hidden: PropTypes.bool, // 是否隐藏
    radioOptions: PropTypes.array.isRequired,
  };

  static defaultProps = {
    hidden: false,
    label: '表单输入框',
    formItemLayout: {
      labelCol: { span: 5 },
      wrapperCol: { span: 15 },
    },
    initialValue: null,
  };

  constructor(props) {
    super(props);
    this.state = {};
  }

  renderRadio = datas => {
    //
    const { radioType } = this.props;
    return datas.map(item => {
      if (radioType === 'RadioButton') {
        return (
          <Radio.Button value={item.value} disabled={item.disabled} key={item.value}>
            {item.label}
          </Radio.Button>
        );
      }

      return (
        <Radio disabled={item.disabled} value={item.value} key={item.value}>
          {item.label}
        </Radio>
      );
    });
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
      required,
      radioOptions,
    } = this.props;

    return hidden ? null : (
      <FormItem {...formItemLayout} label={label} extra={extra}>
        {getFieldDecorator(fieldId, {
          initialValue,
          rules: [
            {
              required,
              message: `${label}必选`,
            },
          ],
        })(<Radio.Group>{this.renderRadio(radioOptions)}</Radio.Group>)}
      </FormItem>
    );
  }
}

export default FormItemRadio;
