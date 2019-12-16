import React, { useState, useEffect } from 'react';
import { Card, Button, Divider, message, Row, Col, Icon, Popconfirm } from 'antd';
import { connect } from 'dva';
import { TableEditV2 } from '@/components/FormWidget';

import styles from './index.less';

const Detail = props => {
  const {
    location: { query },
    storageOptions,
    productSpecOptions,
    dispatch,
    detailData,
  } = props;
  const { id, isOutStock } = query;

  const [dataSource, setDataSource] = useState([]);

  const getInitData = () => {
    dispatch({
      type: 'salesOutStockModel/getDetailEffect',
      payload: { id, isOutStock },
      callback: (data, status, msg) => {
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

  const handleSubmitOK = () => {
    const { contractCarId, productId, productName } = detailData;

    const hasData =
      dataSource.length > 0 &&
      dataSource.filter(item => item.productNumber && item.productNumber && item.storageId).length >
        0;
    if (!hasData) {
      message.warning('请先添加仓库，且每一栏必填');
      return;
    }
    const finishedProductOutDTOS = dataSource.map(item => ({
      contractCarId,
      productId,
      productName,
      productNumber: item.productNumber,
      productSpecId: item.productSpecId,
      productSpecName: productSpecOptions.filter(
        productSpecOptionsItem => productSpecOptionsItem.value === item.productSpecId,
      )[0].label,

      storageId: item.storageId,
      storageName: storageOptions.filter(
        storageOptionsItem => storageOptionsItem.value === item.storageId,
      )[0].label,
    }));

    const payload = {
      bulkOutTonBagListDTO: {
        contractCarId,
        tonBagNumbers: [],
      },
      finishedProductOutDTOS,
    };

    dispatch({
      type: 'salesOutStockModel/saveProductEffect',
      payload,
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

  const handleAddRow = () => {
    const obj = {
      key: `${Math.random()}`,
      productSpecId: '暂无',
      storageId: '暂无',
      productNumber: '暂无',
    };
    const datas = [...dataSource, obj];

    setDataSource(datas);
  };

  const handleDeleteRow = key => {
    const data = dataSource.filter(item => item.key !== key);

    setDataSource(data);
  };

  const columns = [
    {
      title: '规格',
      dataIndex: 'productSpecId',
      editable: true,
      inputType: 'select',
      selectProps: { options: productSpecOptions },
    },
    {
      title: '仓库',
      dataIndex: 'storageId',
      editable: true,
      inputType: 'select',
      selectProps: { options: storageOptions },
    },
    { title: '出库数量', dataIndex: 'productNumber', editable: true, inputType: 'input' },
    {
      title: '操作',
      dataIndex: 'operate',
      render: (text, record) => (
        <Popconfirm
          title="确认删除吗?"
          onConfirm={() => handleDeleteRow(record.key)}
          okText="确定"
          cancelText="取消"
        >
          <Icon type="delete" style={{ color: 'red' }} />
        </Popconfirm>
      ),
    },
  ];

  return (
    <div style={{ marginLeft: 32, marginRight: 32 }}>
      <Card>
        <div>
          <div>
            <h3>业务信息</h3>
            <div className={styles.container}>
              <div className={styles.containerItem}>
                <span className={styles.label}> 状态:</span>
                {String(detailData.putStorage) === '0' ? '待出库' : '已出库'}
              </div>
              <div className={styles.containerItem}>
                <span className={styles.label}> 销售合同号:</span>
                {detailData.contractId}
              </div>
              <div className={styles.containerItem}>
                <span className={styles.label}> 出库时间:</span>
                {detailData.modifyTime}
              </div>

              <div className={styles.containerItem}>
                <span className={styles.label}> 车号:</span>
                {detailData.licensePlateNumber}
              </div>
              <div className={styles.containerItem}>
                <span className={styles.label}> 司机:</span>
                {detailData.driverName}
              </div>
              <div className={styles.containerItem}>
                <span className={styles.label}> 联系方式:</span>
                {detailData.contact}
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
                <span className={styles.label}>总出库数量:</span>
                {detailData.number}
              </div>
            </div>
          </div>
        </div>
        <Divider />
        {Array.isArray(detailData.oneList) && detailData.oneList.length > 0 ? (
          <>
            <div>
              <h3>出库信息</h3>
              {detailData.oneList.map(item => (
                <div className={styles.container}>
                  <div className={styles.containerItem}>
                    <span className={styles.label}> 产品数量:</span>
                    {item.productNumber}
                  </div>
                  <div className={styles.containerItem}>
                    <span className={styles.label}> 产品规格名称:</span>
                    {item.productSpecName}
                  </div>
                  <div className={styles.containerItem}>
                    <span className={styles.label}> 仓库名称:</span>
                    {item.storageName}
                  </div>
                </div>
              ))}
            </div>

            <Divider />
          </>
        ) : null}

        {Array.isArray(detailData.twoList) && detailData.oneList.twoList > 0 ? (
          <>
            <div>
              <h3>吨袋出库信息</h3>
              {detailData.oneList.map(item => (
                <div className={styles.container}>
                  <div className={styles.containerItem}>
                    <span className={styles.label}> 吨袋数量:</span>
                    {item.productNumber}
                  </div>
                  <div className={styles.containerItem}>
                    <span className={styles.label}> 吨袋重量单位:</span>
                    {item.productUnit}
                  </div>
                  <div className={styles.containerItem}>
                    <span className={styles.label}> 仓库名称:</span>
                    {item.storageName}
                  </div>
                  <div className={styles.containerItem}>
                    <span className={styles.label}> 吨袋编号:</span>
                    {item.tonBagNumber}
                  </div>
                </div>
              ))}
            </div>

            <Divider />
          </>
        ) : null}
      </Card>
      {String(detailData.outStorage) === '0' ? (
        <Card style={{ marginTop: 10 }}>
          <h3>出库信息</h3>
          <Row>
            <Col span={24}>
              <TableEditV2
                columns={columns}
                dataSource={dataSource}
                handleTableSve={data => setDataSource(data)}
                footer={() => (
                  <div>
                    <Button
                      type="dashed"
                      icon="plus"
                      style={{
                        width: '100%',
                        height: 40,
                        marginTop: 5,
                        marginBottom: 10,
                      }}
                      onClick={handleAddRow}
                    >
                      添加仓库
                    </Button>
                  </div>
                )}
              />
            </Col>
          </Row>
          <Row>
            <Col offset={22}>
              <Button type="primary" onClick={handleSubmitOK}>
                提交
              </Button>
            </Col>
          </Row>
        </Card>
      ) : null}
    </div>
  );
};

export default connect(state => {
  const { storageOptions, productSpecOptions, detailData } = state.salesOutStockModel;
  const { models } = state.loading;

  return { storageOptions, productSpecOptions, loading: models.salesOutStockModel, detailData };
})(Detail);
