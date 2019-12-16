import React, { Component } from 'react';
import { Form } from 'antd';

const formOptions = {
  name: 'CustomFormHOCForm',
  onValuesChange(props, obj) {
    const { onChange } = props;
    if (onChange) {
      onChange({ ...obj });
    }
  },
  mapPropsToFields: props => {
    const { dataDetail, onChange } = props;
    const result = {};
    if (dataDetail && JSON.stringify(dataDetail) !== '{}') {
      Object.keys(dataDetail).forEach(key => {
        if (dataDetail[key] !== null && dataDetail[key] !== undefined) {
          result[key] = Form.createFormField({
            ...dataDetail[key],
            value: [key].name ? dataDetail[key].value : dataDetail[key],
          });
        }
      });
    }
    return result;
  },
};

const CustomFormHOC = ComponentForm => {
  @Form.create(formOptions)
  class Index extends Component {
    render() {
      return <ComponentForm {...this.props} />;
    }
  }

  return Index;
};

export default CustomFormHOC;
