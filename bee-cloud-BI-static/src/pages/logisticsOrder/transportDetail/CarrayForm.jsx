import React from 'react';
import { Row, Col } from 'antd';
import { FormItemInput, FormItemDatePicker, CustomFormHOC } from '@/components/FormWidget';

const MyForm = props => {
  const {
    form: { getFieldDecorator },
    disabled,
  } = props;
  const formItemLayout = {
    labelCol: { span: 8 },
    wrapperCol: { span: 16 },
  };

  return (
    <Row gutter={8}>
      <Col span={6}>
        <FormItemInput
          getFieldDecorator={getFieldDecorator}
          label="单价"
          fieldId="unitPrice"
          required={false}
          formItemLayout={formItemLayout}
          inputProps={{ disabled }}
        />
      </Col>
      <Col span={6}>
        <FormItemInput
          getFieldDecorator={getFieldDecorator}
          label="运费"
          fieldId="carriage"
          required={false}
          formItemLayout={formItemLayout}
          inputProps={{ disabled }}
        />
      </Col>
      <Col span={6}>
        <FormItemDatePicker
          getFieldDecorator={getFieldDecorator}
          label="出发时间"
          fieldId="departureTime"
          required
          formItemLayout={formItemLayout}
          datePickerProps={{ disabled }}
        />
      </Col>
      <Col span={6}>
        <FormItemDatePicker
          getFieldDecorator={getFieldDecorator}
          label="预计到达时间"
          fieldId="estimateArrivalTime"
          required
          formItemLayout={formItemLayout}
          datePickerProps={{ disabled }}
        />
      </Col>
    </Row>
  );
};

const NewCom = CustomFormHOC(MyForm);

export default NewCom;
