import React, { useState } from 'react';
import { Card, Form, Button, Divider } from 'antd';
import { connect } from 'dva';
import { FormItemSelect, FormItemInput, CustomFormHOC } from '@/components/FormWidget';

import styles from './index.less';

const MyForm = props => {
  const {
    form,
    form: { getFieldDecorator },
    handleSubmitOK,
    storageOptions,
  } = props;
  const formItemLayout = {};

  const [isEditing, setIsEditing] = useState(false);

  const handleSubmit = e => {
    e.preventDefault();
    form.validateFields((err, values) => {
      if (!err && handleSubmitOK) {
        handleSubmitOK(values);
        setIsEditing(false);
      }
    });
  };

  return (
    <Form layout="inline" onSubmit={handleSubmit}>
      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="仓库"
        fieldId="supplierid"
        required
        selectProps={{ options: storageOptions, style: { width: 300 }, disabled: !isEditing }}
        formItemLayout={formItemLayout}
      />

      <FormItemInput
        getFieldDecorator={getFieldDecorator}
        label="入库数量"
        fieldId="a"
        required
        formItemLayout={formItemLayout}
        inputProps={{ placeholder: '默认为收货数量', disabled: !isEditing }}
      />

      <Form.Item>
        {isEditing ? (
          <Button onClick={() => setIsEditing(false)}>取消</Button>
        ) : (
          <Button onClick={() => setIsEditing(true)}>编辑</Button>
        )}
      </Form.Item>
      {isEditing ? (
        <Form.Item>
          <Button type="primary" htmlType="submit">
            提交
          </Button>
        </Form.Item>
      ) : null}
    </Form>
  );
};

const NewMyForm = CustomFormHOC(MyForm);

const Detail = props => {
  const {
    location: { query },
    storageOptions,
  } = props;
  const { a } = query;

  const handleSubmitOK = filds => {
    console.log(filds);
  };
  return (
    <div style={{ marginLeft: 32, marginRight: 32 }}>
      <Card>
        <div>
          <div>
            <h3>业务信息</h3>
            <div className={styles.container}>
              <div className={styles.containerItem}>入库单号:</div>
              <div className={styles.containerItem}>状态:</div>
              <div>采购合同号:</div>

              <div className={styles.containerItem}>车号:</div>
              <div className={styles.containerItem}>处理方式:</div>
            </div>
          </div>
          <Divider />
          <div>
            <h3>产品信息</h3>
            <div className={styles.container}>
              <div className={styles.containerItem}>产品名称:</div>
              <div className={styles.containerItem}>规格:</div>
              <div className={styles.containerItem}>质检结果:</div>

              <div className={styles.containerItem}>收货数量:</div>
              <div className={styles.containerItem}>到厂时间:</div>
              <div className={styles.containerItem}>备注:</div>
            </div>
          </div>
        </div>
      </Card>
      <Card style={{ marginTop: 10 }}>
        <NewMyForm handleSubmitOK={handleSubmitOK} storageOptions={storageOptions} />
      </Card>
    </div>
  );
};

export default connect(state => {
  const { storageOptions } = state.purchaseWarehousingModel;
  const { models } = state.loading;

  return { storageOptions, loading: models.purchaseWarehousingModel };
})(Detail);
