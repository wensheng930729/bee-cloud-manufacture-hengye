import React, { Component } from 'react';
import { Card, Collapse, message, Row, Col, Button, Icon, Popconfirm } from 'antd';
import { connect } from 'dva';
import { cloneDeep, random, isEqual } from 'lodash';
import router from 'umi/router';

import TableEdit from '@/components/FormWidget/tableEdit';
import { modeOfTransportOptions, isToFactoryOptions } from '@/consts/logisticsOrder';

import styles from './index.less';

const { Panel } = Collapse;

@connect(({ LogisticsOrderModel, loading }) => ({
  locationOptions: LogisticsOrderModel.locationOptions,

  contractInfoDTO: LogisticsOrderModel.contractInfoDTO,

  logisticsBatchDTOS: LogisticsOrderModel.logisticsBatchDTOS,
  collapseActivityKey: LogisticsOrderModel.collapseActivityKey,

  transportSectionOBJ: LogisticsOrderModel.transportSectionOBJ,
  transportSectionOBJOrigin: LogisticsOrderModel.transportSectionOBJOrigin,

  loading: loading.effects['LogisticsOrderModel/getLogisticsBatchInfoEffect'],
}))
class Index extends Component {
  constructor(props) {
    super(props);

    this.state = {
      isEditing: false,
    };
  }

  componentDidMount() {
    this.getLogisticsBatchInfo();
  }

  getLogisticsBatchInfo = () => {
    const {
      location: { query },
    } = this.props;
    const { contractBusinessId, businessType } = query;

    const params = {
      contractBusinessId,
      requestType: Number(businessType) === 1 ? 'buyLogisticsBatch' : 'saleLogisticsBatch',
    };

    const { dispatch } = this.props;
    dispatch({
      type: 'LogisticsOrderModel/getLogisticsBatchInfoEffect',
      payload: params,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  };

  // 查看批次下面的详情
  handleCheckDetail = payload => {
    const { transportSectionId } = payload;
    const {
      contractInfoDTO,
      location: { query },
    } = this.props;
    const { businessType } = query;

    router.push({
      pathname: '/logisticsManage/order/transportDetail',
      query: {
        transportSectionId,
        completed: contractInfoDTO.completed,
        requestType: Number(businessType) === 1 ? 'buyTransportSection' : 'saleTransportSection',
      },
    });
  };

  // 是否可编辑--阶段
  handleEditEnable = () => {
    const { isEditing } = this.state;
    this.setState({ isEditing: !isEditing });
  };

  // 表格数据改变事件--阶段
  handleTableSve = payload => {
    const { dispatch, collapseActivityKey, transportSectionOBJ } = this.props;

    const newData = cloneDeep(transportSectionOBJ);
    if (!isEqual(newData[collapseActivityKey], payload)) {
      newData[collapseActivityKey] = payload;

      dispatch({
        type: 'LogisticsOrderModel/cacheSectionListEffect',
        payload: newData,
        callback: (data, status, msg) => {
          if (status === 'error') {
            message.error(msg);
          }
        },
      });
    }
  };

  // 新增--阶段
  handleAddSection = batchId => {
    const { dispatch } = this.props;
    dispatch({
      type: 'LogisticsOrderModel/addSectionListEffect',
      payload: { batchId },
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        } else {
          message.success(msg);
        }
      },
    });
  };

  // 删除--阶段
  handleDeleteSection = obj => {
    const { dispatch } = this.props;
    dispatch({
      type: 'LogisticsOrderModel/deleteSectionListEffect',
      payload: obj,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        } else {
          message.success(msg);
        }
      },
    });
  };

  // 确认保存--阶段
  handleSaveSectionOk = () => {
    try {
      _czc1.push(['_trackEvent', '物流订单', '新增阶段', '', '', '']);
    } catch (error) {}
    const { dispatch } = this.props;
    dispatch({
      type: 'LogisticsOrderModel/saveSectionListEffect',
      payload: {},
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        } else {
          message.success(msg);
          this.handleSaveSectionCancle();
        }
      },
    });
  };

  // 取消保存--阶段
  handleSaveSectionCancle = () => {
    // const { dispatch } = this.props;
    // dispatch({
    //   type: 'LogisticsOrderModel/cacheSectionListEffect',
    //   payload: null,
    //   callback: (data, status, msg) => {
    //     if (status === 'error') {
    //       message.error(msg);
    //     }
    //   },
    // });
    this.getLogisticsBatchInfo();

    this.handleEditEnable();
  };

  genExtra = () => (
    <a>
      <Icon
        type="edit"
        style={{
          fontSize: 20,
          marginRight: 20,
        }}
        onClick={event => {
          // If you don't want click extra trigger collapse, you can prevent this:
          event.stopPropagation();
          this.handleEditEnable();
        }}
      />
    </a>
  );

  handleCollapseChange = collapseActivityKey => {
    const { dispatch } = this.props;
    dispatch({
      type: 'LogisticsOrderModel/cacheCollapseActivityKeyEffect',
      payload: collapseActivityKey,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  };

  // 新增批次
  handleAddBatch = () => {
    try {
      _czc1.push(['_trackEvent', '物流订单', '新增批次', '', '', '']);
    } catch (error) {}
    const { dispatch } = this.props;
    dispatch({
      type: 'LogisticsOrderModel/addBatchListEffect',
      payload: {},
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        } else {
          message.success(msg);
        }
      },
    });
  };

  renderPanel = () => {
    const { isEditing } = this.state;
    const {
      logisticsBatchDTOS,
      locationOptions,
      transportSectionOBJ,
      collapseActivityKey,
      contractInfoDTO,
    } = this.props;

    const columnsForBatch = [
      {
        title: '阶段',
        dataIndex: 'transportSectionName',
        key: 'transportSectionName',
      },
      {
        title: '运输方式',
        dataIndex: 'transportMode',
        key: 'transportMode',
        editable: true,
        selectOptions: modeOfTransportOptions,
        inputType: 'select',
      },

      {
        title: '起始地',
        dataIndex: 'startingPlaceId',
        key: 'startingPlaceId',
        editable: true,
        selectOptions: locationOptions,
        inputType: 'select',
      },
      {
        title: '到达地',
        dataIndex: 'arrivalPlaceId',
        key: 'arrivalPlaceId',
        editable: true,
        selectOptions: locationOptions,
        inputType: 'select',
      },
      {
        title: '是否到厂',
        dataIndex: 'toFactory',
        key: 'toFactory',
        editable: true,
        selectOptions: isToFactoryOptions,
        inputType: 'select',
      },
      {
        title: '操作',
        dataIndex: 'operation',
        key: 'operation',
        render: (text, record) =>
          !record.disabled && !contractInfoDTO.completed ? (
            <Popconfirm
              title="确认删除吗?"
              onConfirm={() =>
                this.handleDeleteSection({
                  collapseActivityKey,
                  transportSectionId: record.transportSectionId,
                })
              }
              okText="确认"
              cancelText="取消"
            >
              <a>删除</a>
            </Popconfirm>
          ) : (
            <a
              onClick={() =>
                this.handleCheckDetail({ transportSectionId: record.transportSectionId })
              }
            >
              查看详情
            </a>
          ),
      },
    ];

    return logisticsBatchDTOS.map(item => {
      const { batchName, batchId } = item;
      return (
        <Panel
          header={`批次:${batchName}`}
          key={batchId}
          extra={!contractInfoDTO.completed && this.genExtra()}
        >
          <TableEdit
            rowKey="transportSectionId"
            dataSource={transportSectionOBJ[item.batchId]}
            columns={columnsForBatch}
            handleTableSve={this.handleTableSve}
            footer={() =>
              isEditing && !contractInfoDTO.completed ? (
                <>
                  <div>
                    <Button
                      type="dashed"
                      onClick={() => this.handleAddSection(item.batchId)}
                      style={{
                        width: '100%',
                        height: 40,
                        marginTop: 5,
                        marginBottom: 10,
                      }}
                    >
                      <Icon type="plus" /> 新增阶段
                    </Button>
                  </div>

                  <div>
                    <Button type="primary" onClick={this.handleSaveSectionOk}>
                      保存
                    </Button>
                    <Button
                      type="default"
                      onClick={this.handleSaveSectionCancle}
                      style={{ display: 'inline-block', marginLeft: 10 }}
                    >
                      取消
                    </Button>
                  </div>
                </>
              ) : null
            }
          />
        </Panel>
      );
    });
  };

  render() {
    const { collapseActivityKey, contractInfoDTO } = this.props;

    return (
      <>
        <Row className={styles.header}>
          <Col span={16} className={styles.left}>
            <h2>物流订单详情</h2>
            <Row className={styles.info}>
              <Col span={10}>
                <span>合同号:</span>
                <span>{contractInfoDTO.contractNum}</span>
              </Col>
              <Col span={10}>
                <span>合同类型:</span>
                <span>{contractInfoDTO.contractBusinessId === 1 ? '采购合同' : '销售合同'}</span>
              </Col>
              <Col span={10}>
                <span>签订时间:</span>
                <span>{contractInfoDTO.signDate}</span>
              </Col>
              <Col span={10}>
                <span>产品名称:</span>
                <span>{contractInfoDTO.productName}</span>
              </Col>
              <Col span={10}>
                <span>合同数量:</span>
                <span>{contractInfoDTO.quantity}</span>
              </Col>
              <Col span={10}>
                <span>采购方式:</span>
                <span>{contractInfoDTO.saleMode === 0 ? '自提' : '包运'}</span>
              </Col>
            </Row>
          </Col>
        </Row>
        <Card style={{ margin: ' 24px 32px' }} title="物流情况">
          <Collapse accordion activeKey={collapseActivityKey} onChange={this.handleCollapseChange}>
            {this.renderPanel()}
          </Collapse>
          {!contractInfoDTO.completed && (
            <Button
              type="dashed"
              onClick={this.handleAddBatch}
              style={{
                width: '98%',
                height: 40,
                marginTop: 20,
                marginRight: '1%',
                marginLeft: '1%',
              }}
            >
              <Icon type="plus" /> 新增批次
            </Button>
          )}
        </Card>
      </>
    );
  }
}

export default Index;
