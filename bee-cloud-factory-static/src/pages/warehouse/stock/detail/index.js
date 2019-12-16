import { Component } from 'react';
import { Button, Card, Table, Divider, Row, Col, message } from 'antd';
import styles from './index.less';
import withRouter from "umi/withRouter";
import BreadCrumb from '../../../../components/BreadCrumb';
import router from 'umi/router';
import {
  getStockDetail
} from '../../services/index';

@withRouter
export default class Index extends Component {
  state = {
    data: {
      code: "",
      detailDTOS: [],
      openingInventoryTime: "",
      remark: "",
    },
  }

  componentDidMount() {
    const { id } = this.props.location.query;
    getStockDetail(id).then(res => {
      if (res && res.code === 1 && res.object) {
        this.setState({
          data: res.object,
        })
      } else {
        message.error(res.message);
      }
    })
  }

  render() {
    const { data } = this.state;
    const columns = [
      {
        title: <span><sup style={{ color: '#f5222d' }}>*</sup>产品</span>,
        dataIndex: 'productName',
        key: 'productName',
      }, {
        title: <span><sup style={{ color: '#f5222d' }}>*</sup>仓库</span>,
        dataIndex: 'repositoryName',
        key: 'repositoryName',
      }, {
        title: <span><sup style={{ color: '#f5222d' }}>*</sup>计量单位</span>,
        dataIndex: 'unit',
        key: 'unit',
      }, {
        title: '化验结果',
        dataIndex: 'testResult',
        key: 'testResult'
      }, {
        title: <span><sup style={{ color: '#f5222d' }}>*</sup>期初数量</span>,
        dataIndex: 'quantity',
        key: 'quantity',
      }, {
        title: <span><sup style={{ color: '#f5222d' }}>*</sup>期初单价</span>,
        dataIndex: 'price',
        key: 'price',
      }, {
        title: <span><sup style={{ color: '#f5222d' }}>*</sup>期初金额</span>,
        dataIndex: 'money',
        key: 'money',
      }
    ]
    return (
      <div>
        <BreadCrumb extra={<Button type="primary" onClick={() => router.goBack()}>返回列表</Button>} />
        <div className={styles.container}>
          <Card title="查看期初库存" bordered={false}>
            <Row>
              <Col span={6}>
                <span><sup style={{ color: '#f5222d' }}>*</sup>单号：{data.code}</span>
              </Col>
              <Col span={6}>
                <span><sup style={{ color: '#f5222d' }}>*</sup>期初日期：{data.openingInventoryTime}</span>
              </Col>
              <Col span={6}>
                <span><sup style={{ color: '#f5222d' }}>*</sup>备注：{data.remark}</span>
              </Col>
            </Row>
            <Divider />
            <p className={styles.tableTitle}>期初明细</p>
            <Table
              rowKey="id"
              columns={columns}
              dataSource={data.detailDTOS}
              pagination={false}
            />
          </Card>
        </div>
      </div>
    )
  }
}