import React, { useState, useEffect } from 'react';
import { Card, Form, Button, Divider, message } from 'antd';
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
        fieldId="storageId"
        required
        selectProps={{ options: storageOptions, style: { width: 300 }, disabled: !isEditing }}
        formItemLayout={formItemLayout}
      />

      <FormItemInput
        getFieldDecorator={getFieldDecorator}
        label="入库数量"
        fieldId="productNumber"
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
    dispatch,
    detailData,
  } = props;
  const { buyProductPendingStorageId } = query;

  const getInitData = () => {
    dispatch({
      type: 'purchaseWarehousingModel/getDetailEffect',
      payload: { buyProductPendingStorageId },
      callback: (_, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  };

  // 这里用于请求 获取详情数据
  useEffect(() => {
    getInitData();
  }, []);

  const handleSubmitOK = filds => {
    const { productId, productName, productUnit } = detailData;

    dispatch({
      type: 'purchaseWarehousingModel/saveProductEffect',
      payload: {
        buyProductPendingStorageId,
        productId,
        productName,

        productUnit,
        storageName: storageOptions.filter(item => item.value === filds.storageId)[0].label,
        ...filds,
      },
      callback: (_, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        } else {
          message.success(msg);
          getInitData();
        }
      },
    });
  };
  return (
    <div style={{ marginLeft: 32, marginRight: 32 }}>
      <Card>
        <div>
          <div>
            <h3>业务信息</h3>
            <div className={styles.container}>
              {/* <div className={styles.containerItem}>
                <span className={styles.label}>入库单号:</span>
                {detailData.buyProductPendingStorageId}
              </div> */}
              <div className={styles.containerItem}>
                <span className={styles.label}> 状态:</span>
                {String(detailData.putStorage) === '0' ? '待入库' : '已入库'}
              </div>
              <div className={styles.containerItem}>
                <span className={styles.label}> 采购合同号:</span>
                {detailData.contractId}
              </div>

              <div className={styles.containerItem}>
                <span className={styles.label}> 车号:</span>

                {detailData.licensePlateNumber}
              </div>
              <div className={styles.containerItem}>
                <span className={styles.label}> 处理方式:</span>
                {String(detailData.processMode) === '0' ? '折价入库' : '确认入库'}
              </div>
            </div>
          </div>
          <Divider />
          <div>
            <h3>产品信息</h3>
            <div className={styles.container}>
              <div className={styles.containerItem}>
                <span className={styles.label}> 产品名称:</span>
                {detailData.productName}
              </div>
              <div className={styles.containerItem}>
                <span className={styles.label}> 规格:</span>

                {detailData.productSpecName}
              </div>
              <div className={styles.containerItem}>
                <span className={styles.label}> 质检结果:</span>

                {String(detailData.analysisResult) === '1' ? '合格' : '不合格'}
              </div>

              <div className={styles.containerItem}>
                <span className={styles.label}>收货数量:</span>

                {detailData.productNumber}
              </div>
              <div className={styles.containerItem}>
                <span className={styles.label}>到厂时间:</span>
                {detailData.arrivalTime}
              </div>
              <div className={styles.containerItem}>
                <span className={styles.label}> 备注:</span>

                {detailData.remark}
              </div>
            </div>
          </div>
        </div>
        <Divider />

        {String(detailData.putStorage) === '1' ? (
          <div>
            <h3>入库信息</h3>
            <div className={styles.container}>
              <div className={styles.containerItem}>
                <span className={styles.label}> 入库数量:</span>
                {detailData.actualProductNumber}
              </div>
              <div className={styles.containerItem}>
                <span className={styles.label}> 仓库:</span>
                {storageOptions.filter(item => item.value === `${detailData.storageId}`)[0].label}
              </div>

              <div className={styles.containerItem}>
                <span className={styles.label}> 入库时间:</span>
                {detailData.storageTime}
              </div>
            </div>
          </div>
        ) : null}
      </Card>
      {String(detailData.putStorage) === '0' ? (
        <Card style={{ marginTop: 10 }}>
          <h3>入库信息</h3>
          <NewMyForm
            handleSubmitOK={handleSubmitOK}
            storageOptions={storageOptions}
            dataDetail={{
              storageId: detailData.storageId ? `${detailData.storageId}` : '',
              productNumber: `${detailData.productNumber}`,
            }}
          />
        </Card>
      ) : null}
    </div>
  );
};

export default connect(state => {
  const { storageOptions, detailData } = state.purchaseWarehousingModel;
  const { models } = state.loading;

  return { storageOptions, loading: models.purchaseWarehousingModel, detailData };
})(Detail);
