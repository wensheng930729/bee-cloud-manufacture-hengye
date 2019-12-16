import React, { Component } from 'react';
import {
  Card,
  Collapse,
  message,
  Button,
  Icon,
  Popconfirm,
  Form,
  Input,
  Row,
  Col,
  Select,
} from 'antd';
import { connect } from 'dva';
import { cloneDeep, isEqual } from 'lodash';

import TableEdit from '@/components/FormWidget/tableEdit';

import CarrayForm from './CarrayForm';
import styles from './index.less';

const { Panel } = Collapse;
const { Option } = Select;

@connect(({ TransportSectionDetailModel, loading }) => ({
  carrayNameOptions: TransportSectionDetailModel.carrayNameOptions,
  transportSectionDetail: TransportSectionDetailModel.transportSectionDetail,

  carrierTransportDTOS: TransportSectionDetailModel.carrierTransportDTOS,
  collapseActivityKey: TransportSectionDetailModel.collapseActivityKey,

  carrierTransportDetailOBJ: TransportSectionDetailModel.carrierTransportDetailOBJ,
  carrierTransportDetailOBJOrigin: TransportSectionDetailModel.carrierTransportDetailOBJOrigin,

  loading: loading.effects['TransportSectionDetailModel/getTransportSectionAllDetailEffect'],
}))
class Index extends Component {
  constructor(props) {
    super(props);

    this.state = {
      isEditing: false,
      completed: 0,
    };
  }

  componentDidMount() {
    this.getInfo();
  }

  getInfo = () => {
    const { dispatch } = this.props;
    const {
      location: { query },
    } = this.props;
    const { requestType, transportSectionId, completed } = query;
    this.setState({
      completed,
    });
    const params = {
      transportSectionId,
      requestType,
    };
    dispatch({
      type: 'TransportSectionDetailModel/getTransportSectionAllDetailEffect',
      payload: params,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  };

  // 是否可编辑--阶段
  handleEditEnable = () => {
    const { isEditing } = this.state;
    this.setState({ isEditing: !isEditing });
  };

  // 表格数据改变事件--运载单位
  handleTableSve = payload => {
    const { dispatch, collapseActivityKey, carrierTransportDetailOBJ } = this.props;

    const newData = cloneDeep(carrierTransportDetailOBJ);
    if (!isEqual(newData[collapseActivityKey], payload)) {
      newData[collapseActivityKey] = payload;

      dispatch({
        type: 'TransportSectionDetailModel/cacheTransportDetailListEffect',
        payload: newData,
        callback: (data, status, msg) => {
          if (status === 'error') {
            message.error(msg);
          }
        },
      });
    }
  };

  // 新增--运载单位
  handleAddCarrierTransport = id => {
    try {
      _czc1.push(['_trackEvent', '物流订单', '新增运载单位', '', '', '']);
    } catch (error) {}
    const { dispatch } = this.props;
    dispatch({
      type: 'TransportSectionDetailModel/addTransportDetailListEffect',
      payload: { id },
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        } else {
          message.success(msg);
        }
      },
    });
  };

  // 删除--运载单位
  handleDeleteSection = obj => {
    const { dispatch } = this.props;
    dispatch({
      type: 'TransportSectionDetailModel/deleteTransportDetailListEffect',
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

  // 确认保存--承运商及运载单位
  handleSaveCarrayOk = () => {
    const { dispatch } = this.props;
    dispatch({
      type: 'TransportSectionDetailModel/saveCarrayListEffect',
      payload: {},
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        } else {
          message.success(msg);
          this.handleSaveCarrayCancle();
        }
      },
    });
  };

  // 取消保存--阶段
  handleSaveCarrayCancle = () => {
    this.getInfo();
    this.handleEditEnable();
  };

  handleCollapseChange = collapseActivityKey => {
    const { dispatch } = this.props;
    dispatch({
      type: 'TransportSectionDetailModel/cacheCollapseActivityKeyEffect',
      payload: collapseActivityKey,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  };

  // 新增 承运商
  handleAddCarray = () => {
    const { dispatch } = this.props;
    dispatch({
      type: 'TransportSectionDetailModel/addCarrayListEffect',
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

  handleCarrayFormChange = (field, id) => {
    const { carrierTransportDTOS } = this.props;

    const currentData = carrierTransportDTOS.map(item => {
      let newItem = { ...item };
      if (item.id === id) {
        newItem = { ...item, ...field };
      }
      return newItem;
    });
    const { dispatch } = this.props;
    dispatch({
      type: 'TransportSectionDetailModel/cacheCarrayListReduce',
      payload: currentData,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        } else {
          message.success(msg);
        }
      },
    });
  };

  rendercarrierNameInput = obj => {
    const { completed } = this.state;
    const { carrierId: value, id, disabled } = obj;
    const { carrayNameOptions } = this.props;

    const formItemLayout = {
      labelCol: { span: 6 },
      wrapperCol: { span: 18 },
    };
    return (
      <div style={{ display: 'flex', justifyContent: 'space-between' }}>
        <div
          style={{ width: 300 }}
          onClick={event => {
            event.stopPropagation();
          }}
        >
          <Form.Item label="承运方" {...formItemLayout} style={{ marginBottom: 0 }}>
            <Select
              placeholder="请选择承运商"
              value={`${value}`}
              disabled={disabled}
              style={{ width: '100%' }}
              onSelect={data => {
                this.handleCarrayFormChange({ carrierId: data }, id);
              }}
            >
              {carrayNameOptions.map(item => (
                <Option value={item.value}>{item.label}</Option>
              ))}
            </Select>
          </Form.Item>
        </div>
        <div
          style={{ width: 40 }}
          onClick={event => {
            event.stopPropagation();
          }}
        >
          {Number(completed) === 0 && (
            <a className={styles.editStyle}>
              <Icon
                type="edit"
                style={{
                  fontSize: 20,
                }}
                onClick={event => {
                  // If you don't want click extra trigger collapse, you can prevent this:
                  event.stopPropagation();
                  this.handleEditEnable();
                }}
              />
            </a>
          )}
        </div>
      </div>
    );
  };

  renderPanel = () => {
    const { isEditing, completed } = this.state;
    const {
      carrierTransportDTOS,

      carrierTransportDetailOBJ,
      collapseActivityKey,
    } = this.props;

    const columnsForCarray = [
      {
        title: <span className={styles.itemRequired}>运载单位编号</span>,
        dataIndex: 'trainNumber',
        key: 'trainNumber',
        editable: true,
        inputType: 'input',
      },
      {
        title: '司机',
        dataIndex: 'driver',
        key: 'driver',
        editable: true,
        inputType: 'input',
      },

      {
        title: '手机号',
        dataIndex: 'contact',
        key: 'contact',
        editable: true,

        inputType: 'input',
      },
      {
        title: <span className={styles.itemRequired}>吨位</span>,
        dataIndex: 'cargoWeight',
        key: 'cargoWeight',

        editable: true,

        inputType: 'input',
      },
      {
        title: '操作',
        dataIndex: 'operation',
        key: 'operation',
        render: (text, record) =>
          !record.disabled ? (
            <Popconfirm
              title="确认删除吗?"
              onConfirm={() =>
                this.handleDeleteSection({
                  collapseActivityKey,
                  id: record.id,
                })
              }
              okText="确认"
              cancelText="取消"
            >
              <a>删除</a>
            </Popconfirm>
          ) : null,
      },
    ];

    return carrierTransportDTOS.map(item => {
      const { carrierId, id, unitPrice, carriage, departureTime, estimateArrivalTime } = item;
      const formValue = {
        unitPrice,
        carriage,
        departureTime,
        estimateArrivalTime,
      };

      const disabled = !(id && `${id}`.includes('id'));

      const carrierNameInputParams = {
        carrierId,
        id,
        disabled,
      };

      return (
        <Panel header={this.rendercarrierNameInput(carrierNameInputParams)} key={id}>
          <TableEdit
            title={() => (
              <CarrayForm
                dataDetail={formValue}
                onChange={field => this.handleCarrayFormChange(field, id)}
                disabled={disabled}
              />
            )}
            dataSource={carrierTransportDetailOBJ[item.id] || []}
            columns={columnsForCarray}
            handleTableSve={this.handleTableSve}
            footer={() =>
              (isEditing || !disabled) && Number(completed) === 0 ? (
                <>
                  <div>
                    <Button
                      type="dashed"
                      onClick={() => this.handleAddCarrierTransport(item.id)}
                      style={{
                        width: '100%',
                        height: 40,
                        marginTop: 5,
                        marginBottom: 10,
                      }}
                    >
                      <Icon type="plus" /> 新增运载单位
                    </Button>
                  </div>

                  <div>
                    <Button type="primary" onClick={this.handleSaveCarrayOk}>
                      保存
                    </Button>
                    <Button
                      type="default"
                      onClick={this.handleSaveCarrayCancle}
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
    const { completed } = this.state;
    const { collapseActivityKey, transportSectionDetail } = this.props;

    return (
      <>
        <Row className={styles.header}>
          <Col span={16} className={styles.left}>
            <div className={styles.title}>批次-阶段详情</div>
            <Row className={styles.info}>
              <Col span={10}>
                <span>运输方式:</span>
                <span>{transportSectionDetail.transportModeName}</span>
              </Col>
              <Col span={10}>
                <span>是否到厂:</span>
                <span>{transportSectionDetail.toFactory === 1 ? '是' : '否'}</span>
              </Col>
              <Col span={10}>
                <span>起始地:</span>
                <span>{transportSectionDetail.startingPlace}</span>
              </Col>
              <Col span={10}>
                <span>到达地:</span>
                <span>{transportSectionDetail.arrivalPlace}</span>
              </Col>
            </Row>
          </Col>
        </Row>
        <Card style={{ margin: ' 24px 32px' }} title="承运商情况">
          <Collapse accordion activeKey={collapseActivityKey} onChange={this.handleCollapseChange}>
            {this.renderPanel()}
          </Collapse>
          {Number(completed) === 0 && (
            <Button
              type="dashed"
              onClick={this.handleAddCarray}
              style={{
                width: '98%',
                height: 40,
                marginTop: 20,
                marginRight: '1%',
                marginLeft: '1%',
              }}
            >
              <Icon type="plus" /> 新增承运商
            </Button>
          )}
        </Card>
      </>
    );
  }
}

export default Index;
